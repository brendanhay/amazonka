{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateIPSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an 'IPSet' , which you use to specify which web requests you want to allow or block based on the IP addresses that the requests originate from. For example, if you're receiving a lot of requests from one or more individual IP addresses or one or more ranges of IP addresses and you want to block the requests, you can create an @IPSet@ that contains those IP addresses and then configure AWS WAF to block the requests.
--
--
-- To create and configure an @IPSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateIPSet@ request.
--
--     * Submit a @CreateIPSet@ request.
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--     * Submit an @UpdateIPSet@ request to specify the IP addresses that you want AWS WAF to watch for.
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.CreateIPSet
    (
    -- * Creating a Request
      createIPSet
    , CreateIPSet
    -- * Request Lenses
    , cisName
    , cisChangeToken

    -- * Destructuring the Response
    , createIPSetResponse
    , CreateIPSetResponse
    -- * Response Lenses
    , cisrsChangeToken
    , cisrsIPSet
    , cisrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'createIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { _cisName        :: !Text
  , _cisChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisName' - A friendly name or description of the 'IPSet' . You can't change @Name@ after you create the @IPSet@ .
--
-- * 'cisChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
createIPSet
    :: Text -- ^ 'cisName'
    -> Text -- ^ 'cisChangeToken'
    -> CreateIPSet
createIPSet pName_ pChangeToken_ =
  CreateIPSet' {_cisName = pName_, _cisChangeToken = pChangeToken_}


-- | A friendly name or description of the 'IPSet' . You can't change @Name@ after you create the @IPSet@ .
cisName :: Lens' CreateIPSet Text
cisName = lens _cisName (\ s a -> s{_cisName = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
cisChangeToken :: Lens' CreateIPSet Text
cisChangeToken = lens _cisChangeToken (\ s a -> s{_cisChangeToken = a})

instance AWSRequest CreateIPSet where
        type Rs CreateIPSet = CreateIPSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 CreateIPSetResponse' <$>
                   (x .?> "ChangeToken") <*> (x .?> "IPSet") <*>
                     (pure (fromEnum s)))

instance Hashable CreateIPSet where

instance NFData CreateIPSet where

instance ToHeaders CreateIPSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.CreateIPSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateIPSet where
        toJSON CreateIPSet'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cisName),
                  Just ("ChangeToken" .= _cisChangeToken)])

instance ToPath CreateIPSet where
        toPath = const "/"

instance ToQuery CreateIPSet where
        toQuery = const mempty

-- | /See:/ 'createIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { _cisrsChangeToken    :: !(Maybe Text)
  , _cisrsIPSet          :: !(Maybe IPSet)
  , _cisrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisrsChangeToken' - The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'cisrsIPSet' - The 'IPSet' returned in the @CreateIPSet@ response.
--
-- * 'cisrsResponseStatus' - -- | The response status code.
createIPSetResponse
    :: Int -- ^ 'cisrsResponseStatus'
    -> CreateIPSetResponse
createIPSetResponse pResponseStatus_ =
  CreateIPSetResponse'
    { _cisrsChangeToken = Nothing
    , _cisrsIPSet = Nothing
    , _cisrsResponseStatus = pResponseStatus_
    }


-- | The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
cisrsChangeToken :: Lens' CreateIPSetResponse (Maybe Text)
cisrsChangeToken = lens _cisrsChangeToken (\ s a -> s{_cisrsChangeToken = a})

-- | The 'IPSet' returned in the @CreateIPSet@ response.
cisrsIPSet :: Lens' CreateIPSetResponse (Maybe IPSet)
cisrsIPSet = lens _cisrsIPSet (\ s a -> s{_cisrsIPSet = a})

-- | -- | The response status code.
cisrsResponseStatus :: Lens' CreateIPSetResponse Int
cisrsResponseStatus = lens _cisrsResponseStatus (\ s a -> s{_cisrsResponseStatus = a})

instance NFData CreateIPSetResponse where

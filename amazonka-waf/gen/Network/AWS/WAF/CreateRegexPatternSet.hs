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
-- Module      : Network.AWS.WAF.CreateRegexPatternSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @RegexPatternSet@ . You then use 'UpdateRegexPatternSet' to specify the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
--
-- To create and configure a @RegexPatternSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRegexPatternSet@ request.
--
--     * Submit a @CreateRegexPatternSet@ request.
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
--     * Submit an 'UpdateRegexPatternSet' request to specify the string that you want AWS WAF to watch for.
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.CreateRegexPatternSet
    (
    -- * Creating a Request
      createRegexPatternSet
    , CreateRegexPatternSet
    -- * Request Lenses
    , crpsName
    , crpsChangeToken

    -- * Destructuring the Response
    , createRegexPatternSetResponse
    , CreateRegexPatternSetResponse
    -- * Response Lenses
    , crpsrsRegexPatternSet
    , crpsrsChangeToken
    , crpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'createRegexPatternSet' smart constructor.
data CreateRegexPatternSet = CreateRegexPatternSet'
  { _crpsName        :: !Text
  , _crpsChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRegexPatternSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crpsName' - A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
--
-- * 'crpsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
createRegexPatternSet
    :: Text -- ^ 'crpsName'
    -> Text -- ^ 'crpsChangeToken'
    -> CreateRegexPatternSet
createRegexPatternSet pName_ pChangeToken_ =
  CreateRegexPatternSet' {_crpsName = pName_, _crpsChangeToken = pChangeToken_}


-- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
crpsName :: Lens' CreateRegexPatternSet Text
crpsName = lens _crpsName (\ s a -> s{_crpsName = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
crpsChangeToken :: Lens' CreateRegexPatternSet Text
crpsChangeToken = lens _crpsChangeToken (\ s a -> s{_crpsChangeToken = a})

instance AWSRequest CreateRegexPatternSet where
        type Rs CreateRegexPatternSet =
             CreateRegexPatternSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 CreateRegexPatternSetResponse' <$>
                   (x .?> "RegexPatternSet") <*> (x .?> "ChangeToken")
                     <*> (pure (fromEnum s)))

instance Hashable CreateRegexPatternSet where

instance NFData CreateRegexPatternSet where

instance ToHeaders CreateRegexPatternSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.CreateRegexPatternSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRegexPatternSet where
        toJSON CreateRegexPatternSet'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _crpsName),
                  Just ("ChangeToken" .= _crpsChangeToken)])

instance ToPath CreateRegexPatternSet where
        toPath = const "/"

instance ToQuery CreateRegexPatternSet where
        toQuery = const mempty

-- | /See:/ 'createRegexPatternSetResponse' smart constructor.
data CreateRegexPatternSetResponse = CreateRegexPatternSetResponse'
  { _crpsrsRegexPatternSet :: !(Maybe RegexPatternSet)
  , _crpsrsChangeToken     :: !(Maybe Text)
  , _crpsrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRegexPatternSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crpsrsRegexPatternSet' - A 'RegexPatternSet' that contains no objects.
--
-- * 'crpsrsChangeToken' - The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'crpsrsResponseStatus' - -- | The response status code.
createRegexPatternSetResponse
    :: Int -- ^ 'crpsrsResponseStatus'
    -> CreateRegexPatternSetResponse
createRegexPatternSetResponse pResponseStatus_ =
  CreateRegexPatternSetResponse'
    { _crpsrsRegexPatternSet = Nothing
    , _crpsrsChangeToken = Nothing
    , _crpsrsResponseStatus = pResponseStatus_
    }


-- | A 'RegexPatternSet' that contains no objects.
crpsrsRegexPatternSet :: Lens' CreateRegexPatternSetResponse (Maybe RegexPatternSet)
crpsrsRegexPatternSet = lens _crpsrsRegexPatternSet (\ s a -> s{_crpsrsRegexPatternSet = a})

-- | The @ChangeToken@ that you used to submit the @CreateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
crpsrsChangeToken :: Lens' CreateRegexPatternSetResponse (Maybe Text)
crpsrsChangeToken = lens _crpsrsChangeToken (\ s a -> s{_crpsrsChangeToken = a})

-- | -- | The response status code.
crpsrsResponseStatus :: Lens' CreateRegexPatternSetResponse Int
crpsrsResponseStatus = lens _crpsrsResponseStatus (\ s a -> s{_crpsrsResponseStatus = a})

instance NFData CreateRegexPatternSetResponse where

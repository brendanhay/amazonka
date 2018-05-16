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
-- Module      : Network.AWS.WAF.CreateSizeConstraintSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @SizeConstraintSet@ . You then use 'UpdateSizeConstraintSet' to identify the part of a web request that you want AWS WAF to check for length, such as the length of the @User-Agent@ header or the length of the query string. For example, you can create a @SizeConstraintSet@ that matches any requests that have a query string that is longer than 100 bytes. You can then configure AWS WAF to reject those requests.
--
--
-- To create and configure a @SizeConstraintSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateSizeConstraintSet@ request.
--
--     * Submit a @CreateSizeConstraintSet@ request.
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateSizeConstraintSet@ request.
--
--     * Submit an 'UpdateSizeConstraintSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.CreateSizeConstraintSet
    (
    -- * Creating a Request
      createSizeConstraintSet
    , CreateSizeConstraintSet
    -- * Request Lenses
    , cscsName
    , cscsChangeToken

    -- * Destructuring the Response
    , createSizeConstraintSetResponse
    , CreateSizeConstraintSetResponse
    -- * Response Lenses
    , cscsrsSizeConstraintSet
    , cscsrsChangeToken
    , cscsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'createSizeConstraintSet' smart constructor.
data CreateSizeConstraintSet = CreateSizeConstraintSet'
  { _cscsName        :: !Text
  , _cscsChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSizeConstraintSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscsName' - A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
--
-- * 'cscsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
createSizeConstraintSet
    :: Text -- ^ 'cscsName'
    -> Text -- ^ 'cscsChangeToken'
    -> CreateSizeConstraintSet
createSizeConstraintSet pName_ pChangeToken_ =
  CreateSizeConstraintSet'
    {_cscsName = pName_, _cscsChangeToken = pChangeToken_}


-- | A friendly name or description of the 'SizeConstraintSet' . You can't change @Name@ after you create a @SizeConstraintSet@ .
cscsName :: Lens' CreateSizeConstraintSet Text
cscsName = lens _cscsName (\ s a -> s{_cscsName = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
cscsChangeToken :: Lens' CreateSizeConstraintSet Text
cscsChangeToken = lens _cscsChangeToken (\ s a -> s{_cscsChangeToken = a})

instance AWSRequest CreateSizeConstraintSet where
        type Rs CreateSizeConstraintSet =
             CreateSizeConstraintSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 CreateSizeConstraintSetResponse' <$>
                   (x .?> "SizeConstraintSet") <*> (x .?> "ChangeToken")
                     <*> (pure (fromEnum s)))

instance Hashable CreateSizeConstraintSet where

instance NFData CreateSizeConstraintSet where

instance ToHeaders CreateSizeConstraintSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.CreateSizeConstraintSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSizeConstraintSet where
        toJSON CreateSizeConstraintSet'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cscsName),
                  Just ("ChangeToken" .= _cscsChangeToken)])

instance ToPath CreateSizeConstraintSet where
        toPath = const "/"

instance ToQuery CreateSizeConstraintSet where
        toQuery = const mempty

-- | /See:/ 'createSizeConstraintSetResponse' smart constructor.
data CreateSizeConstraintSetResponse = CreateSizeConstraintSetResponse'
  { _cscsrsSizeConstraintSet :: !(Maybe SizeConstraintSet)
  , _cscsrsChangeToken       :: !(Maybe Text)
  , _cscsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSizeConstraintSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscsrsSizeConstraintSet' - A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
--
-- * 'cscsrsChangeToken' - The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'cscsrsResponseStatus' - -- | The response status code.
createSizeConstraintSetResponse
    :: Int -- ^ 'cscsrsResponseStatus'
    -> CreateSizeConstraintSetResponse
createSizeConstraintSetResponse pResponseStatus_ =
  CreateSizeConstraintSetResponse'
    { _cscsrsSizeConstraintSet = Nothing
    , _cscsrsChangeToken = Nothing
    , _cscsrsResponseStatus = pResponseStatus_
    }


-- | A 'SizeConstraintSet' that contains no @SizeConstraint@ objects.
cscsrsSizeConstraintSet :: Lens' CreateSizeConstraintSetResponse (Maybe SizeConstraintSet)
cscsrsSizeConstraintSet = lens _cscsrsSizeConstraintSet (\ s a -> s{_cscsrsSizeConstraintSet = a})

-- | The @ChangeToken@ that you used to submit the @CreateSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
cscsrsChangeToken :: Lens' CreateSizeConstraintSetResponse (Maybe Text)
cscsrsChangeToken = lens _cscsrsChangeToken (\ s a -> s{_cscsrsChangeToken = a})

-- | -- | The response status code.
cscsrsResponseStatus :: Lens' CreateSizeConstraintSetResponse Int
cscsrsResponseStatus = lens _cscsrsResponseStatus (\ s a -> s{_cscsrsResponseStatus = a})

instance NFData CreateSizeConstraintSetResponse where

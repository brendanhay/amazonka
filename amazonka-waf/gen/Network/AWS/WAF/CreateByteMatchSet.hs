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
-- Module      : Network.AWS.WAF.CreateByteMatchSet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'ByteMatchSet'. You then use < UpdateByteMatchSet> to identify the part of a web request that you want AWS WAF to inspect, such as the values of the 'User-Agent' header or the query string. For example, you can create a 'ByteMatchSet' that matches any requests with 'User-Agent' headers that contain the string 'BadBot'. You can then configure AWS WAF to reject those requests.
--
-- To create and configure a 'ByteMatchSet', perform the following steps:
--
-- 1.  Use < GetChangeToken> to get the change token that you provide in the 'ChangeToken' parameter of a 'CreateByteMatchSet' request.
-- 2.  Submit a 'CreateByteMatchSet' request.
-- 3.  Use 'GetChangeToken' to get the change token that you provide in the 'ChangeToken' parameter of an 'UpdateByteMatchSet' request.
-- 4.  Submit an < UpdateByteMatchSet> request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.CreateByteMatchSet
    (
    -- * Creating a Request
      createByteMatchSet
    , CreateByteMatchSet
    -- * Request Lenses
    , cbmsName
    , cbmsChangeToken

    -- * Destructuring the Response
    , createByteMatchSetResponse
    , CreateByteMatchSetResponse
    -- * Response Lenses
    , cbmsrsByteMatchSet
    , cbmsrsChangeToken
    , cbmsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WAF.Types
import           Network.AWS.WAF.Types.Product

-- | /See:/ 'createByteMatchSet' smart constructor.
data CreateByteMatchSet = CreateByteMatchSet'
    { _cbmsName        :: !Text
    , _cbmsChangeToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateByteMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbmsName'
--
-- * 'cbmsChangeToken'
createByteMatchSet
    :: Text -- ^ 'cbmsName'
    -> Text -- ^ 'cbmsChangeToken'
    -> CreateByteMatchSet
createByteMatchSet pName_ pChangeToken_ =
    CreateByteMatchSet'
    { _cbmsName = pName_
    , _cbmsChangeToken = pChangeToken_
    }

-- | A friendly name or description of the < ByteMatchSet>. You can\'t change 'Name' after you create a 'ByteMatchSet'.
cbmsName :: Lens' CreateByteMatchSet Text
cbmsName = lens _cbmsName (\ s a -> s{_cbmsName = a});

-- | The value returned by the most recent call to < GetChangeToken>.
cbmsChangeToken :: Lens' CreateByteMatchSet Text
cbmsChangeToken = lens _cbmsChangeToken (\ s a -> s{_cbmsChangeToken = a});

instance AWSRequest CreateByteMatchSet where
        type Rs CreateByteMatchSet =
             CreateByteMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 CreateByteMatchSetResponse' <$>
                   (x .?> "ByteMatchSet") <*> (x .?> "ChangeToken") <*>
                     (pure (fromEnum s)))

instance Hashable CreateByteMatchSet

instance NFData CreateByteMatchSet

instance ToHeaders CreateByteMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.CreateByteMatchSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateByteMatchSet where
        toJSON CreateByteMatchSet'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cbmsName),
                  Just ("ChangeToken" .= _cbmsChangeToken)])

instance ToPath CreateByteMatchSet where
        toPath = const "/"

instance ToQuery CreateByteMatchSet where
        toQuery = const mempty

-- | /See:/ 'createByteMatchSetResponse' smart constructor.
data CreateByteMatchSetResponse = CreateByteMatchSetResponse'
    { _cbmsrsByteMatchSet   :: !(Maybe ByteMatchSet)
    , _cbmsrsChangeToken    :: !(Maybe Text)
    , _cbmsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateByteMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbmsrsByteMatchSet'
--
-- * 'cbmsrsChangeToken'
--
-- * 'cbmsrsResponseStatus'
createByteMatchSetResponse
    :: Int -- ^ 'cbmsrsResponseStatus'
    -> CreateByteMatchSetResponse
createByteMatchSetResponse pResponseStatus_ =
    CreateByteMatchSetResponse'
    { _cbmsrsByteMatchSet = Nothing
    , _cbmsrsChangeToken = Nothing
    , _cbmsrsResponseStatus = pResponseStatus_
    }

-- | A < ByteMatchSet> that contains no 'ByteMatchTuple' objects.
cbmsrsByteMatchSet :: Lens' CreateByteMatchSetResponse (Maybe ByteMatchSet)
cbmsrsByteMatchSet = lens _cbmsrsByteMatchSet (\ s a -> s{_cbmsrsByteMatchSet = a});

-- | The 'ChangeToken' that you used to submit the 'CreateByteMatchSet' request. You can also use this value to query the status of the request. For more information, see < GetChangeTokenStatus>.
cbmsrsChangeToken :: Lens' CreateByteMatchSetResponse (Maybe Text)
cbmsrsChangeToken = lens _cbmsrsChangeToken (\ s a -> s{_cbmsrsChangeToken = a});

-- | The response status code.
cbmsrsResponseStatus :: Lens' CreateByteMatchSetResponse Int
cbmsrsResponseStatus = lens _cbmsrsResponseStatus (\ s a -> s{_cbmsrsResponseStatus = a});

instance NFData CreateByteMatchSetResponse

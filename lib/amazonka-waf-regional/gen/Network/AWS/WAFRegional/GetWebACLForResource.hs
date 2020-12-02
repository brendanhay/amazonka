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
-- Module      : Network.AWS.WAFRegional.GetWebACLForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the web ACL for the specified resource.
--
--
module Network.AWS.WAFRegional.GetWebACLForResource
    (
    -- * Creating a Request
      getWebACLForResource
    , GetWebACLForResource
    -- * Request Lenses
    , gwafrResourceARN

    -- * Destructuring the Response
    , getWebACLForResourceResponse
    , GetWebACLForResourceResponse
    -- * Response Lenses
    , gwafrrsWebACLSummary
    , gwafrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'getWebACLForResource' smart constructor.
newtype GetWebACLForResource = GetWebACLForResource'
  { _gwafrResourceARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetWebACLForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwafrResourceARN' - The ARN (Amazon Resource Name) of the resource for which to get the web ACL.
getWebACLForResource
    :: Text -- ^ 'gwafrResourceARN'
    -> GetWebACLForResource
getWebACLForResource pResourceARN_ =
  GetWebACLForResource' {_gwafrResourceARN = pResourceARN_}


-- | The ARN (Amazon Resource Name) of the resource for which to get the web ACL.
gwafrResourceARN :: Lens' GetWebACLForResource Text
gwafrResourceARN = lens _gwafrResourceARN (\ s a -> s{_gwafrResourceARN = a})

instance AWSRequest GetWebACLForResource where
        type Rs GetWebACLForResource =
             GetWebACLForResourceResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 GetWebACLForResourceResponse' <$>
                   (x .?> "WebACLSummary") <*> (pure (fromEnum s)))

instance Hashable GetWebACLForResource where

instance NFData GetWebACLForResource where

instance ToHeaders GetWebACLForResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.GetWebACLForResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetWebACLForResource where
        toJSON GetWebACLForResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _gwafrResourceARN)])

instance ToPath GetWebACLForResource where
        toPath = const "/"

instance ToQuery GetWebACLForResource where
        toQuery = const mempty

-- | /See:/ 'getWebACLForResourceResponse' smart constructor.
data GetWebACLForResourceResponse = GetWebACLForResourceResponse'
  { _gwafrrsWebACLSummary  :: !(Maybe WebACLSummary)
  , _gwafrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetWebACLForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwafrrsWebACLSummary' - Information about the web ACL that you specified in the @GetWebACLForResource@ request. If there is no associated resource, a null WebACLSummary is returned.
--
-- * 'gwafrrsResponseStatus' - -- | The response status code.
getWebACLForResourceResponse
    :: Int -- ^ 'gwafrrsResponseStatus'
    -> GetWebACLForResourceResponse
getWebACLForResourceResponse pResponseStatus_ =
  GetWebACLForResourceResponse'
    {_gwafrrsWebACLSummary = Nothing, _gwafrrsResponseStatus = pResponseStatus_}


-- | Information about the web ACL that you specified in the @GetWebACLForResource@ request. If there is no associated resource, a null WebACLSummary is returned.
gwafrrsWebACLSummary :: Lens' GetWebACLForResourceResponse (Maybe WebACLSummary)
gwafrrsWebACLSummary = lens _gwafrrsWebACLSummary (\ s a -> s{_gwafrrsWebACLSummary = a})

-- | -- | The response status code.
gwafrrsResponseStatus :: Lens' GetWebACLForResourceResponse Int
gwafrrsResponseStatus = lens _gwafrrsResponseStatus (\ s a -> s{_gwafrrsResponseStatus = a})

instance NFData GetWebACLForResourceResponse where

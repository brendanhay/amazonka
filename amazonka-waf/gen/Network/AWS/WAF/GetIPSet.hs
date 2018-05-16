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
-- Module      : Network.AWS.WAF.GetIPSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'IPSet' that is specified by @IPSetId@ .
--
--
module Network.AWS.WAF.GetIPSet
    (
    -- * Creating a Request
      getIPSet
    , GetIPSet
    -- * Request Lenses
    , gisIPSetId

    -- * Destructuring the Response
    , getIPSetResponse
    , GetIPSetResponse
    -- * Response Lenses
    , gisrsIPSet
    , gisrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'getIPSet' smart constructor.
newtype GetIPSet = GetIPSet'
  { _gisIPSetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisIPSetId' - The @IPSetId@ of the 'IPSet' that you want to get. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
getIPSet
    :: Text -- ^ 'gisIPSetId'
    -> GetIPSet
getIPSet pIPSetId_ = GetIPSet' {_gisIPSetId = pIPSetId_}


-- | The @IPSetId@ of the 'IPSet' that you want to get. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
gisIPSetId :: Lens' GetIPSet Text
gisIPSetId = lens _gisIPSetId (\ s a -> s{_gisIPSetId = a})

instance AWSRequest GetIPSet where
        type Rs GetIPSet = GetIPSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 GetIPSetResponse' <$>
                   (x .?> "IPSet") <*> (pure (fromEnum s)))

instance Hashable GetIPSet where

instance NFData GetIPSet where

instance ToHeaders GetIPSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.GetIPSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetIPSet where
        toJSON GetIPSet'{..}
          = object
              (catMaybes [Just ("IPSetId" .= _gisIPSetId)])

instance ToPath GetIPSet where
        toPath = const "/"

instance ToQuery GetIPSet where
        toQuery = const mempty

-- | /See:/ 'getIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { _gisrsIPSet          :: !(Maybe IPSet)
  , _gisrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisrsIPSet' - Information about the 'IPSet' that you specified in the @GetIPSet@ request. For more information, see the following topics:     * 'IPSet' : Contains @IPSetDescriptors@ , @IPSetId@ , and @Name@      * @IPSetDescriptors@ : Contains an array of 'IPSetDescriptor' objects. Each @IPSetDescriptor@ object contains @Type@ and @Value@
--
-- * 'gisrsResponseStatus' - -- | The response status code.
getIPSetResponse
    :: Int -- ^ 'gisrsResponseStatus'
    -> GetIPSetResponse
getIPSetResponse pResponseStatus_ =
  GetIPSetResponse'
    {_gisrsIPSet = Nothing, _gisrsResponseStatus = pResponseStatus_}


-- | Information about the 'IPSet' that you specified in the @GetIPSet@ request. For more information, see the following topics:     * 'IPSet' : Contains @IPSetDescriptors@ , @IPSetId@ , and @Name@      * @IPSetDescriptors@ : Contains an array of 'IPSetDescriptor' objects. Each @IPSetDescriptor@ object contains @Type@ and @Value@
gisrsIPSet :: Lens' GetIPSetResponse (Maybe IPSet)
gisrsIPSet = lens _gisrsIPSet (\ s a -> s{_gisrsIPSet = a})

-- | -- | The response status code.
gisrsResponseStatus :: Lens' GetIPSetResponse Int
gisrsResponseStatus = lens _gisrsResponseStatus (\ s a -> s{_gisrsResponseStatus = a})

instance NFData GetIPSetResponse where

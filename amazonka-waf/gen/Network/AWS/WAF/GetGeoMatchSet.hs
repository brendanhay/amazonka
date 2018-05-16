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
-- Module      : Network.AWS.WAF.GetGeoMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'GeoMatchSet' that is specified by @GeoMatchSetId@ .
--
--
module Network.AWS.WAF.GetGeoMatchSet
    (
    -- * Creating a Request
      getGeoMatchSet
    , GetGeoMatchSet
    -- * Request Lenses
    , ggmsGeoMatchSetId

    -- * Destructuring the Response
    , getGeoMatchSetResponse
    , GetGeoMatchSetResponse
    -- * Response Lenses
    , ggmsrsGeoMatchSet
    , ggmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'getGeoMatchSet' smart constructor.
newtype GetGeoMatchSet = GetGeoMatchSet'
  { _ggmsGeoMatchSetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGeoMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggmsGeoMatchSetId' - The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to get. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
getGeoMatchSet
    :: Text -- ^ 'ggmsGeoMatchSetId'
    -> GetGeoMatchSet
getGeoMatchSet pGeoMatchSetId_ =
  GetGeoMatchSet' {_ggmsGeoMatchSetId = pGeoMatchSetId_}


-- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to get. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
ggmsGeoMatchSetId :: Lens' GetGeoMatchSet Text
ggmsGeoMatchSetId = lens _ggmsGeoMatchSetId (\ s a -> s{_ggmsGeoMatchSetId = a})

instance AWSRequest GetGeoMatchSet where
        type Rs GetGeoMatchSet = GetGeoMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 GetGeoMatchSetResponse' <$>
                   (x .?> "GeoMatchSet") <*> (pure (fromEnum s)))

instance Hashable GetGeoMatchSet where

instance NFData GetGeoMatchSet where

instance ToHeaders GetGeoMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.GetGeoMatchSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetGeoMatchSet where
        toJSON GetGeoMatchSet'{..}
          = object
              (catMaybes
                 [Just ("GeoMatchSetId" .= _ggmsGeoMatchSetId)])

instance ToPath GetGeoMatchSet where
        toPath = const "/"

instance ToQuery GetGeoMatchSet where
        toQuery = const mempty

-- | /See:/ 'getGeoMatchSetResponse' smart constructor.
data GetGeoMatchSetResponse = GetGeoMatchSetResponse'
  { _ggmsrsGeoMatchSet    :: !(Maybe GeoMatchSet)
  , _ggmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGeoMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggmsrsGeoMatchSet' - Information about the 'GeoMatchSet' that you specified in the @GetGeoMatchSet@ request. This includes the @Type@ , which for a @GeoMatchContraint@ is always @Country@ , as well as the @Value@ , which is the identifier for a specific country.
--
-- * 'ggmsrsResponseStatus' - -- | The response status code.
getGeoMatchSetResponse
    :: Int -- ^ 'ggmsrsResponseStatus'
    -> GetGeoMatchSetResponse
getGeoMatchSetResponse pResponseStatus_ =
  GetGeoMatchSetResponse'
    {_ggmsrsGeoMatchSet = Nothing, _ggmsrsResponseStatus = pResponseStatus_}


-- | Information about the 'GeoMatchSet' that you specified in the @GetGeoMatchSet@ request. This includes the @Type@ , which for a @GeoMatchContraint@ is always @Country@ , as well as the @Value@ , which is the identifier for a specific country.
ggmsrsGeoMatchSet :: Lens' GetGeoMatchSetResponse (Maybe GeoMatchSet)
ggmsrsGeoMatchSet = lens _ggmsrsGeoMatchSet (\ s a -> s{_ggmsrsGeoMatchSet = a})

-- | -- | The response status code.
ggmsrsResponseStatus :: Lens' GetGeoMatchSetResponse Int
ggmsrsResponseStatus = lens _ggmsrsResponseStatus (\ s a -> s{_ggmsrsResponseStatus = a})

instance NFData GetGeoMatchSetResponse where

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
-- Module      : Network.AWS.APIGateway.GetBasePathMappings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of 'BasePathMapping' resources.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetBasePathMappings
    (
    -- * Creating a Request
      getBasePathMappings
    , GetBasePathMappings
    -- * Request Lenses
    , gLimit
    , gPosition
    , gDomainName

    -- * Destructuring the Response
    , getBasePathMappingsResponse
    , GetBasePathMappingsResponse
    -- * Response Lenses
    , gbpmrsItems
    , gbpmrsPosition
    , gbpmrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to get information about a collection of 'BasePathMapping' resources.
--
--
--
-- /See:/ 'getBasePathMappings' smart constructor.
data GetBasePathMappings = GetBasePathMappings'
  { _gLimit      :: !(Maybe Int)
  , _gPosition   :: !(Maybe Text)
  , _gDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBasePathMappings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gPosition' - The current pagination position in the paged result set.
--
-- * 'gDomainName' - [Required] The domain name of a 'BasePathMapping' resource.
getBasePathMappings
    :: Text -- ^ 'gDomainName'
    -> GetBasePathMappings
getBasePathMappings pDomainName_ =
  GetBasePathMappings'
    {_gLimit = Nothing, _gPosition = Nothing, _gDomainName = pDomainName_}


-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gLimit :: Lens' GetBasePathMappings (Maybe Int)
gLimit = lens _gLimit (\ s a -> s{_gLimit = a})

-- | The current pagination position in the paged result set.
gPosition :: Lens' GetBasePathMappings (Maybe Text)
gPosition = lens _gPosition (\ s a -> s{_gPosition = a})

-- | [Required] The domain name of a 'BasePathMapping' resource.
gDomainName :: Lens' GetBasePathMappings Text
gDomainName = lens _gDomainName (\ s a -> s{_gDomainName = a})

instance AWSPager GetBasePathMappings where
        page rq rs
          | stop (rs ^. gbpmrsPosition) = Nothing
          | stop (rs ^. gbpmrsItems) = Nothing
          | otherwise =
            Just $ rq & gPosition .~ rs ^. gbpmrsPosition

instance AWSRequest GetBasePathMappings where
        type Rs GetBasePathMappings =
             GetBasePathMappingsResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetBasePathMappingsResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetBasePathMappings where

instance NFData GetBasePathMappings where

instance ToHeaders GetBasePathMappings where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetBasePathMappings where
        toPath GetBasePathMappings'{..}
          = mconcat
              ["/domainnames/", toBS _gDomainName,
               "/basepathmappings"]

instance ToQuery GetBasePathMappings where
        toQuery GetBasePathMappings'{..}
          = mconcat
              ["limit" =: _gLimit, "position" =: _gPosition]

-- | Represents a collection of 'BasePathMapping' resources.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names>
--
-- /See:/ 'getBasePathMappingsResponse' smart constructor.
data GetBasePathMappingsResponse = GetBasePathMappingsResponse'
  { _gbpmrsItems          :: !(Maybe [BasePathMapping])
  , _gbpmrsPosition       :: !(Maybe Text)
  , _gbpmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBasePathMappingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpmrsItems' - The current page of elements from this collection.
--
-- * 'gbpmrsPosition' - Undocumented member.
--
-- * 'gbpmrsResponseStatus' - -- | The response status code.
getBasePathMappingsResponse
    :: Int -- ^ 'gbpmrsResponseStatus'
    -> GetBasePathMappingsResponse
getBasePathMappingsResponse pResponseStatus_ =
  GetBasePathMappingsResponse'
    { _gbpmrsItems = Nothing
    , _gbpmrsPosition = Nothing
    , _gbpmrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
gbpmrsItems :: Lens' GetBasePathMappingsResponse [BasePathMapping]
gbpmrsItems = lens _gbpmrsItems (\ s a -> s{_gbpmrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
gbpmrsPosition :: Lens' GetBasePathMappingsResponse (Maybe Text)
gbpmrsPosition = lens _gbpmrsPosition (\ s a -> s{_gbpmrsPosition = a})

-- | -- | The response status code.
gbpmrsResponseStatus :: Lens' GetBasePathMappingsResponse Int
gbpmrsResponseStatus = lens _gbpmrsResponseStatus (\ s a -> s{_gbpmrsResponseStatus = a})

instance NFData GetBasePathMappingsResponse where

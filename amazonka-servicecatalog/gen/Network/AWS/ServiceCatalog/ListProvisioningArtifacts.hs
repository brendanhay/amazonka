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
-- Module      : Network.AWS.ServiceCatalog.ListProvisioningArtifacts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all provisioning artifacts (also known as versions) for the specified product.
--
--
module Network.AWS.ServiceCatalog.ListProvisioningArtifacts
    (
    -- * Creating a Request
      listProvisioningArtifacts
    , ListProvisioningArtifacts
    -- * Request Lenses
    , lpaAcceptLanguage
    , lpaProductId

    -- * Destructuring the Response
    , listProvisioningArtifactsResponse
    , ListProvisioningArtifactsResponse
    -- * Response Lenses
    , lrsNextPageToken
    , lrsProvisioningArtifactDetails
    , lrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listProvisioningArtifacts' smart constructor.
data ListProvisioningArtifacts = ListProvisioningArtifacts'
  { _lpaAcceptLanguage :: !(Maybe Text)
  , _lpaProductId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProvisioningArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lpaProductId' - The product identifier.
listProvisioningArtifacts
    :: Text -- ^ 'lpaProductId'
    -> ListProvisioningArtifacts
listProvisioningArtifacts pProductId_ =
  ListProvisioningArtifacts'
    {_lpaAcceptLanguage = Nothing, _lpaProductId = pProductId_}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lpaAcceptLanguage :: Lens' ListProvisioningArtifacts (Maybe Text)
lpaAcceptLanguage = lens _lpaAcceptLanguage (\ s a -> s{_lpaAcceptLanguage = a})

-- | The product identifier.
lpaProductId :: Lens' ListProvisioningArtifacts Text
lpaProductId = lens _lpaProductId (\ s a -> s{_lpaProductId = a})

instance AWSRequest ListProvisioningArtifacts where
        type Rs ListProvisioningArtifacts =
             ListProvisioningArtifactsResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListProvisioningArtifactsResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProvisioningArtifactDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListProvisioningArtifacts where

instance NFData ListProvisioningArtifacts where

instance ToHeaders ListProvisioningArtifacts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListProvisioningArtifacts"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListProvisioningArtifacts where
        toJSON ListProvisioningArtifacts'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _lpaAcceptLanguage,
                  Just ("ProductId" .= _lpaProductId)])

instance ToPath ListProvisioningArtifacts where
        toPath = const "/"

instance ToQuery ListProvisioningArtifacts where
        toQuery = const mempty

-- | /See:/ 'listProvisioningArtifactsResponse' smart constructor.
data ListProvisioningArtifactsResponse = ListProvisioningArtifactsResponse'
  { _lrsNextPageToken               :: !(Maybe Text)
  , _lrsProvisioningArtifactDetails :: !(Maybe [ProvisioningArtifactDetail])
  , _lrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProvisioningArtifactsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lrsProvisioningArtifactDetails' - Information about the provisioning artifacts.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listProvisioningArtifactsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListProvisioningArtifactsResponse
listProvisioningArtifactsResponse pResponseStatus_ =
  ListProvisioningArtifactsResponse'
    { _lrsNextPageToken = Nothing
    , _lrsProvisioningArtifactDetails = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lrsNextPageToken :: Lens' ListProvisioningArtifactsResponse (Maybe Text)
lrsNextPageToken = lens _lrsNextPageToken (\ s a -> s{_lrsNextPageToken = a})

-- | Information about the provisioning artifacts.
lrsProvisioningArtifactDetails :: Lens' ListProvisioningArtifactsResponse [ProvisioningArtifactDetail]
lrsProvisioningArtifactDetails = lens _lrsProvisioningArtifactDetails (\ s a -> s{_lrsProvisioningArtifactDetails = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListProvisioningArtifactsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListProvisioningArtifactsResponse
         where

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
-- Module      : Network.AWS.ServiceCatalog.CreateProvisioningArtifact
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new provisioning artifact for the specified product. This operation does not work with a product that has been shared with you.
--
--
-- See the bottom of this topic for an example JSON request.
--
module Network.AWS.ServiceCatalog.CreateProvisioningArtifact
    (
    -- * Creating a Request
      createProvisioningArtifact
    , CreateProvisioningArtifact
    -- * Request Lenses
    , cpaAcceptLanguage
    , cpaProductId
    , cpaParameters
    , cpaIdempotencyToken

    -- * Destructuring the Response
    , createProvisioningArtifactResponse
    , CreateProvisioningArtifactResponse
    -- * Response Lenses
    , cparsStatus
    , cparsInfo
    , cparsProvisioningArtifactDetail
    , cparsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'createProvisioningArtifact' smart constructor.
data CreateProvisioningArtifact = CreateProvisioningArtifact'
    { _cpaAcceptLanguage   :: !(Maybe Text)
    , _cpaProductId        :: !Text
    , _cpaParameters       :: !ProvisioningArtifactProperties
    , _cpaIdempotencyToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpaAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'cpaProductId' - The product identifier.
--
-- * 'cpaParameters' - The parameters to use when creating the new provisioning artifact.
--
-- * 'cpaIdempotencyToken' - A token to disambiguate duplicate requests. You can create multiple resources using the same input in multiple requests, provided that you also specify a different idempotency token for each request.
createProvisioningArtifact
    :: Text -- ^ 'cpaProductId'
    -> ProvisioningArtifactProperties -- ^ 'cpaParameters'
    -> Text -- ^ 'cpaIdempotencyToken'
    -> CreateProvisioningArtifact
createProvisioningArtifact pProductId_ pParameters_ pIdempotencyToken_ =
    CreateProvisioningArtifact'
    { _cpaAcceptLanguage = Nothing
    , _cpaProductId = pProductId_
    , _cpaParameters = pParameters_
    , _cpaIdempotencyToken = pIdempotencyToken_
    }

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
cpaAcceptLanguage :: Lens' CreateProvisioningArtifact (Maybe Text)
cpaAcceptLanguage = lens _cpaAcceptLanguage (\ s a -> s{_cpaAcceptLanguage = a});

-- | The product identifier.
cpaProductId :: Lens' CreateProvisioningArtifact Text
cpaProductId = lens _cpaProductId (\ s a -> s{_cpaProductId = a});

-- | The parameters to use when creating the new provisioning artifact.
cpaParameters :: Lens' CreateProvisioningArtifact ProvisioningArtifactProperties
cpaParameters = lens _cpaParameters (\ s a -> s{_cpaParameters = a});

-- | A token to disambiguate duplicate requests. You can create multiple resources using the same input in multiple requests, provided that you also specify a different idempotency token for each request.
cpaIdempotencyToken :: Lens' CreateProvisioningArtifact Text
cpaIdempotencyToken = lens _cpaIdempotencyToken (\ s a -> s{_cpaIdempotencyToken = a});

instance AWSRequest CreateProvisioningArtifact where
        type Rs CreateProvisioningArtifact =
             CreateProvisioningArtifactResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 CreateProvisioningArtifactResponse' <$>
                   (x .?> "Status") <*> (x .?> "Info" .!@ mempty) <*>
                     (x .?> "ProvisioningArtifactDetail")
                     <*> (pure (fromEnum s)))

instance Hashable CreateProvisioningArtifact

instance NFData CreateProvisioningArtifact

instance ToHeaders CreateProvisioningArtifact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.CreateProvisioningArtifact"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProvisioningArtifact where
        toJSON CreateProvisioningArtifact'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _cpaAcceptLanguage,
                  Just ("ProductId" .= _cpaProductId),
                  Just ("Parameters" .= _cpaParameters),
                  Just ("IdempotencyToken" .= _cpaIdempotencyToken)])

instance ToPath CreateProvisioningArtifact where
        toPath = const "/"

instance ToQuery CreateProvisioningArtifact where
        toQuery = const mempty

-- | /See:/ 'createProvisioningArtifactResponse' smart constructor.
data CreateProvisioningArtifactResponse = CreateProvisioningArtifactResponse'
    { _cparsStatus                     :: !(Maybe RequestStatus)
    , _cparsInfo                       :: !(Maybe (Map Text Text))
    , _cparsProvisioningArtifactDetail :: !(Maybe ProvisioningArtifactDetail)
    , _cparsResponseStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cparsStatus' - The status of the current request.
--
-- * 'cparsInfo' - Additional information about the creation request for the provisioning artifact.
--
-- * 'cparsProvisioningArtifactDetail' - The resulting detailed provisioning artifact information.
--
-- * 'cparsResponseStatus' - -- | The response status code.
createProvisioningArtifactResponse
    :: Int -- ^ 'cparsResponseStatus'
    -> CreateProvisioningArtifactResponse
createProvisioningArtifactResponse pResponseStatus_ =
    CreateProvisioningArtifactResponse'
    { _cparsStatus = Nothing
    , _cparsInfo = Nothing
    , _cparsProvisioningArtifactDetail = Nothing
    , _cparsResponseStatus = pResponseStatus_
    }

-- | The status of the current request.
cparsStatus :: Lens' CreateProvisioningArtifactResponse (Maybe RequestStatus)
cparsStatus = lens _cparsStatus (\ s a -> s{_cparsStatus = a});

-- | Additional information about the creation request for the provisioning artifact.
cparsInfo :: Lens' CreateProvisioningArtifactResponse (HashMap Text Text)
cparsInfo = lens _cparsInfo (\ s a -> s{_cparsInfo = a}) . _Default . _Map;

-- | The resulting detailed provisioning artifact information.
cparsProvisioningArtifactDetail :: Lens' CreateProvisioningArtifactResponse (Maybe ProvisioningArtifactDetail)
cparsProvisioningArtifactDetail = lens _cparsProvisioningArtifactDetail (\ s a -> s{_cparsProvisioningArtifactDetail = a});

-- | -- | The response status code.
cparsResponseStatus :: Lens' CreateProvisioningArtifactResponse Int
cparsResponseStatus = lens _cparsResponseStatus (\ s a -> s{_cparsResponseStatus = a});

instance NFData CreateProvisioningArtifactResponse

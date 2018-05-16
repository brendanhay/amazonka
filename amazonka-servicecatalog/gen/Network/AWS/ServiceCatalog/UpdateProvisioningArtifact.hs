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
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified provisioning artifact (also known as a version) for the specified product.
--
--
-- You cannot update a provisioning artifact for a product that was shared with you.
--
module Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
    (
    -- * Creating a Request
      updateProvisioningArtifact
    , UpdateProvisioningArtifact
    -- * Request Lenses
    , upaActive
    , upaName
    , upaAcceptLanguage
    , upaDescription
    , upaProductId
    , upaProvisioningArtifactId

    -- * Destructuring the Response
    , updateProvisioningArtifactResponse
    , UpdateProvisioningArtifactResponse
    -- * Response Lenses
    , uparsStatus
    , uparsInfo
    , uparsProvisioningArtifactDetail
    , uparsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'updateProvisioningArtifact' smart constructor.
data UpdateProvisioningArtifact = UpdateProvisioningArtifact'
  { _upaActive                 :: !(Maybe Bool)
  , _upaName                   :: !(Maybe Text)
  , _upaAcceptLanguage         :: !(Maybe Text)
  , _upaDescription            :: !(Maybe Text)
  , _upaProductId              :: !Text
  , _upaProvisioningArtifactId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upaActive' - Indicates whether the product version is active.
--
-- * 'upaName' - The updated name of the provisioning artifact.
--
-- * 'upaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'upaDescription' - The updated description of the provisioning artifact.
--
-- * 'upaProductId' - The product identifier.
--
-- * 'upaProvisioningArtifactId' - The identifier of the provisioning artifact.
updateProvisioningArtifact
    :: Text -- ^ 'upaProductId'
    -> Text -- ^ 'upaProvisioningArtifactId'
    -> UpdateProvisioningArtifact
updateProvisioningArtifact pProductId_ pProvisioningArtifactId_ =
  UpdateProvisioningArtifact'
    { _upaActive = Nothing
    , _upaName = Nothing
    , _upaAcceptLanguage = Nothing
    , _upaDescription = Nothing
    , _upaProductId = pProductId_
    , _upaProvisioningArtifactId = pProvisioningArtifactId_
    }


-- | Indicates whether the product version is active.
upaActive :: Lens' UpdateProvisioningArtifact (Maybe Bool)
upaActive = lens _upaActive (\ s a -> s{_upaActive = a})

-- | The updated name of the provisioning artifact.
upaName :: Lens' UpdateProvisioningArtifact (Maybe Text)
upaName = lens _upaName (\ s a -> s{_upaName = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
upaAcceptLanguage :: Lens' UpdateProvisioningArtifact (Maybe Text)
upaAcceptLanguage = lens _upaAcceptLanguage (\ s a -> s{_upaAcceptLanguage = a})

-- | The updated description of the provisioning artifact.
upaDescription :: Lens' UpdateProvisioningArtifact (Maybe Text)
upaDescription = lens _upaDescription (\ s a -> s{_upaDescription = a})

-- | The product identifier.
upaProductId :: Lens' UpdateProvisioningArtifact Text
upaProductId = lens _upaProductId (\ s a -> s{_upaProductId = a})

-- | The identifier of the provisioning artifact.
upaProvisioningArtifactId :: Lens' UpdateProvisioningArtifact Text
upaProvisioningArtifactId = lens _upaProvisioningArtifactId (\ s a -> s{_upaProvisioningArtifactId = a})

instance AWSRequest UpdateProvisioningArtifact where
        type Rs UpdateProvisioningArtifact =
             UpdateProvisioningArtifactResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 UpdateProvisioningArtifactResponse' <$>
                   (x .?> "Status") <*> (x .?> "Info" .!@ mempty) <*>
                     (x .?> "ProvisioningArtifactDetail")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateProvisioningArtifact where

instance NFData UpdateProvisioningArtifact where

instance ToHeaders UpdateProvisioningArtifact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.UpdateProvisioningArtifact"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateProvisioningArtifact where
        toJSON UpdateProvisioningArtifact'{..}
          = object
              (catMaybes
                 [("Active" .=) <$> _upaActive,
                  ("Name" .=) <$> _upaName,
                  ("AcceptLanguage" .=) <$> _upaAcceptLanguage,
                  ("Description" .=) <$> _upaDescription,
                  Just ("ProductId" .= _upaProductId),
                  Just
                    ("ProvisioningArtifactId" .=
                       _upaProvisioningArtifactId)])

instance ToPath UpdateProvisioningArtifact where
        toPath = const "/"

instance ToQuery UpdateProvisioningArtifact where
        toQuery = const mempty

-- | /See:/ 'updateProvisioningArtifactResponse' smart constructor.
data UpdateProvisioningArtifactResponse = UpdateProvisioningArtifactResponse'
  { _uparsStatus                     :: !(Maybe RequestStatus)
  , _uparsInfo                       :: !(Maybe (Map Text Text))
  , _uparsProvisioningArtifactDetail :: !(Maybe ProvisioningArtifactDetail)
  , _uparsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uparsStatus' - The status of the current request.
--
-- * 'uparsInfo' - The URL of the CloudFormation template in Amazon S3.
--
-- * 'uparsProvisioningArtifactDetail' - Information about the provisioning artifact.
--
-- * 'uparsResponseStatus' - -- | The response status code.
updateProvisioningArtifactResponse
    :: Int -- ^ 'uparsResponseStatus'
    -> UpdateProvisioningArtifactResponse
updateProvisioningArtifactResponse pResponseStatus_ =
  UpdateProvisioningArtifactResponse'
    { _uparsStatus = Nothing
    , _uparsInfo = Nothing
    , _uparsProvisioningArtifactDetail = Nothing
    , _uparsResponseStatus = pResponseStatus_
    }


-- | The status of the current request.
uparsStatus :: Lens' UpdateProvisioningArtifactResponse (Maybe RequestStatus)
uparsStatus = lens _uparsStatus (\ s a -> s{_uparsStatus = a})

-- | The URL of the CloudFormation template in Amazon S3.
uparsInfo :: Lens' UpdateProvisioningArtifactResponse (HashMap Text Text)
uparsInfo = lens _uparsInfo (\ s a -> s{_uparsInfo = a}) . _Default . _Map

-- | Information about the provisioning artifact.
uparsProvisioningArtifactDetail :: Lens' UpdateProvisioningArtifactResponse (Maybe ProvisioningArtifactDetail)
uparsProvisioningArtifactDetail = lens _uparsProvisioningArtifactDetail (\ s a -> s{_uparsProvisioningArtifactDetail = a})

-- | -- | The response status code.
uparsResponseStatus :: Lens' UpdateProvisioningArtifactResponse Int
uparsResponseStatus = lens _uparsResponseStatus (\ s a -> s{_uparsResponseStatus = a})

instance NFData UpdateProvisioningArtifactResponse
         where

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
-- Module      : Network.AWS.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates multiple self-service actions with provisioning artifacts.
--
--
module Network.AWS.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact
    (
    -- * Creating a Request
      batchAssociateServiceActionWithProvisioningArtifact
    , BatchAssociateServiceActionWithProvisioningArtifact
    -- * Request Lenses
    , basawpaAcceptLanguage
    , basawpaServiceActionAssociations

    -- * Destructuring the Response
    , batchAssociateServiceActionWithProvisioningArtifactResponse
    , BatchAssociateServiceActionWithProvisioningArtifactResponse
    -- * Response Lenses
    , basawparsFailedServiceActionAssociations
    , basawparsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'batchAssociateServiceActionWithProvisioningArtifact' smart constructor.
data BatchAssociateServiceActionWithProvisioningArtifact = BatchAssociateServiceActionWithProvisioningArtifact'
  { _basawpaAcceptLanguage            :: !(Maybe Text)
  , _basawpaServiceActionAssociations :: !(List1 ServiceActionAssociation)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAssociateServiceActionWithProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'basawpaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'basawpaServiceActionAssociations' - One or more associations, each consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
batchAssociateServiceActionWithProvisioningArtifact
    :: NonEmpty ServiceActionAssociation -- ^ 'basawpaServiceActionAssociations'
    -> BatchAssociateServiceActionWithProvisioningArtifact
batchAssociateServiceActionWithProvisioningArtifact pServiceActionAssociations_ =
  BatchAssociateServiceActionWithProvisioningArtifact'
    { _basawpaAcceptLanguage = Nothing
    , _basawpaServiceActionAssociations = _List1 # pServiceActionAssociations_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
basawpaAcceptLanguage :: Lens' BatchAssociateServiceActionWithProvisioningArtifact (Maybe Text)
basawpaAcceptLanguage = lens _basawpaAcceptLanguage (\ s a -> s{_basawpaAcceptLanguage = a})

-- | One or more associations, each consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
basawpaServiceActionAssociations :: Lens' BatchAssociateServiceActionWithProvisioningArtifact (NonEmpty ServiceActionAssociation)
basawpaServiceActionAssociations = lens _basawpaServiceActionAssociations (\ s a -> s{_basawpaServiceActionAssociations = a}) . _List1

instance AWSRequest
           BatchAssociateServiceActionWithProvisioningArtifact
         where
        type Rs
               BatchAssociateServiceActionWithProvisioningArtifact
             =
             BatchAssociateServiceActionWithProvisioningArtifactResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 BatchAssociateServiceActionWithProvisioningArtifactResponse'
                   <$>
                   (x .?> "FailedServiceActionAssociations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable
           BatchAssociateServiceActionWithProvisioningArtifact
         where

instance NFData
           BatchAssociateServiceActionWithProvisioningArtifact
         where

instance ToHeaders
           BatchAssociateServiceActionWithProvisioningArtifact
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.BatchAssociateServiceActionWithProvisioningArtifact"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           BatchAssociateServiceActionWithProvisioningArtifact
         where
        toJSON
          BatchAssociateServiceActionWithProvisioningArtifact'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _basawpaAcceptLanguage,
                  Just
                    ("ServiceActionAssociations" .=
                       _basawpaServiceActionAssociations)])

instance ToPath
           BatchAssociateServiceActionWithProvisioningArtifact
         where
        toPath = const "/"

instance ToQuery
           BatchAssociateServiceActionWithProvisioningArtifact
         where
        toQuery = const mempty

-- | /See:/ 'batchAssociateServiceActionWithProvisioningArtifactResponse' smart constructor.
data BatchAssociateServiceActionWithProvisioningArtifactResponse = BatchAssociateServiceActionWithProvisioningArtifactResponse'
  { _basawparsFailedServiceActionAssociations :: !(Maybe [FailedServiceActionAssociation])
  , _basawparsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAssociateServiceActionWithProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'basawparsFailedServiceActionAssociations' - An object that contains a list of errors, along with information to help you identify the self-service action.
--
-- * 'basawparsResponseStatus' - -- | The response status code.
batchAssociateServiceActionWithProvisioningArtifactResponse
    :: Int -- ^ 'basawparsResponseStatus'
    -> BatchAssociateServiceActionWithProvisioningArtifactResponse
batchAssociateServiceActionWithProvisioningArtifactResponse pResponseStatus_ =
  BatchAssociateServiceActionWithProvisioningArtifactResponse'
    { _basawparsFailedServiceActionAssociations = Nothing
    , _basawparsResponseStatus = pResponseStatus_
    }


-- | An object that contains a list of errors, along with information to help you identify the self-service action.
basawparsFailedServiceActionAssociations :: Lens' BatchAssociateServiceActionWithProvisioningArtifactResponse [FailedServiceActionAssociation]
basawparsFailedServiceActionAssociations = lens _basawparsFailedServiceActionAssociations (\ s a -> s{_basawparsFailedServiceActionAssociations = a}) . _Default . _Coerce

-- | -- | The response status code.
basawparsResponseStatus :: Lens' BatchAssociateServiceActionWithProvisioningArtifactResponse Int
basawparsResponseStatus = lens _basawparsResponseStatus (\ s a -> s{_basawparsResponseStatus = a})

instance NFData
           BatchAssociateServiceActionWithProvisioningArtifactResponse
         where

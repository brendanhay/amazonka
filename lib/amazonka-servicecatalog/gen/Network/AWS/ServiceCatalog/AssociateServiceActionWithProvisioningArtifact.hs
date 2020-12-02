{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a self-service action with a provisioning artifact.
module Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
  ( -- * Creating a Request
    associateServiceActionWithProvisioningArtifact,
    AssociateServiceActionWithProvisioningArtifact,

    -- * Request Lenses
    asawpaAcceptLanguage,
    asawpaProductId,
    asawpaProvisioningArtifactId,
    asawpaServiceActionId,

    -- * Destructuring the Response
    associateServiceActionWithProvisioningArtifactResponse,
    AssociateServiceActionWithProvisioningArtifactResponse,

    -- * Response Lenses
    asawparsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'associateServiceActionWithProvisioningArtifact' smart constructor.
data AssociateServiceActionWithProvisioningArtifact = AssociateServiceActionWithProvisioningArtifact'
  { _asawpaAcceptLanguage ::
      !( Maybe
           Text
       ),
    _asawpaProductId ::
      !Text,
    _asawpaProvisioningArtifactId ::
      !Text,
    _asawpaServiceActionId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AssociateServiceActionWithProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asawpaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'asawpaProductId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- * 'asawpaProvisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- * 'asawpaServiceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
associateServiceActionWithProvisioningArtifact ::
  -- | 'asawpaProductId'
  Text ->
  -- | 'asawpaProvisioningArtifactId'
  Text ->
  -- | 'asawpaServiceActionId'
  Text ->
  AssociateServiceActionWithProvisioningArtifact
associateServiceActionWithProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_
  pServiceActionId_ =
    AssociateServiceActionWithProvisioningArtifact'
      { _asawpaAcceptLanguage =
          Nothing,
        _asawpaProductId = pProductId_,
        _asawpaProvisioningArtifactId =
          pProvisioningArtifactId_,
        _asawpaServiceActionId = pServiceActionId_
      }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
asawpaAcceptLanguage :: Lens' AssociateServiceActionWithProvisioningArtifact (Maybe Text)
asawpaAcceptLanguage = lens _asawpaAcceptLanguage (\s a -> s {_asawpaAcceptLanguage = a})

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
asawpaProductId :: Lens' AssociateServiceActionWithProvisioningArtifact Text
asawpaProductId = lens _asawpaProductId (\s a -> s {_asawpaProductId = a})

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
asawpaProvisioningArtifactId :: Lens' AssociateServiceActionWithProvisioningArtifact Text
asawpaProvisioningArtifactId = lens _asawpaProvisioningArtifactId (\s a -> s {_asawpaProvisioningArtifactId = a})

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
asawpaServiceActionId :: Lens' AssociateServiceActionWithProvisioningArtifact Text
asawpaServiceActionId = lens _asawpaServiceActionId (\s a -> s {_asawpaServiceActionId = a})

instance AWSRequest AssociateServiceActionWithProvisioningArtifact where
  type
    Rs AssociateServiceActionWithProvisioningArtifact =
      AssociateServiceActionWithProvisioningArtifactResponse
  request = postJSON serviceCatalog
  response =
    receiveEmpty
      ( \s h x ->
          AssociateServiceActionWithProvisioningArtifactResponse'
            <$> (pure (fromEnum s))
      )

instance Hashable AssociateServiceActionWithProvisioningArtifact

instance NFData AssociateServiceActionWithProvisioningArtifact

instance ToHeaders AssociateServiceActionWithProvisioningArtifact where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.AssociateServiceActionWithProvisioningArtifact" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssociateServiceActionWithProvisioningArtifact where
  toJSON AssociateServiceActionWithProvisioningArtifact' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _asawpaAcceptLanguage,
            Just ("ProductId" .= _asawpaProductId),
            Just ("ProvisioningArtifactId" .= _asawpaProvisioningArtifactId),
            Just ("ServiceActionId" .= _asawpaServiceActionId)
          ]
      )

instance ToPath AssociateServiceActionWithProvisioningArtifact where
  toPath = const "/"

instance ToQuery AssociateServiceActionWithProvisioningArtifact where
  toQuery = const mempty

-- | /See:/ 'associateServiceActionWithProvisioningArtifactResponse' smart constructor.
newtype AssociateServiceActionWithProvisioningArtifactResponse = AssociateServiceActionWithProvisioningArtifactResponse'
  { _asawparsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AssociateServiceActionWithProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asawparsResponseStatus' - -- | The response status code.
associateServiceActionWithProvisioningArtifactResponse ::
  -- | 'asawparsResponseStatus'
  Int ->
  AssociateServiceActionWithProvisioningArtifactResponse
associateServiceActionWithProvisioningArtifactResponse
  pResponseStatus_ =
    AssociateServiceActionWithProvisioningArtifactResponse'
      { _asawparsResponseStatus =
          pResponseStatus_
      }

-- | -- | The response status code.
asawparsResponseStatus :: Lens' AssociateServiceActionWithProvisioningArtifactResponse Int
asawparsResponseStatus = lens _asawparsResponseStatus (\s a -> s {_asawparsResponseStatus = a})

instance
  NFData
    AssociateServiceActionWithProvisioningArtifactResponse

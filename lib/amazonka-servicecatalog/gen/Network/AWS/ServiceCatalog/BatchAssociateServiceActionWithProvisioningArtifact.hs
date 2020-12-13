{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates multiple self-service actions with provisioning artifacts.
module Network.AWS.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact
  ( -- * Creating a request
    BatchAssociateServiceActionWithProvisioningArtifact (..),
    mkBatchAssociateServiceActionWithProvisioningArtifact,

    -- ** Request lenses
    basawpaServiceActionAssociations,
    basawpaAcceptLanguage,

    -- * Destructuring the response
    BatchAssociateServiceActionWithProvisioningArtifactResponse (..),
    mkBatchAssociateServiceActionWithProvisioningArtifactResponse,

    -- ** Response lenses
    basawparsFailedServiceActionAssociations,
    basawparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkBatchAssociateServiceActionWithProvisioningArtifact' smart constructor.
data BatchAssociateServiceActionWithProvisioningArtifact = BatchAssociateServiceActionWithProvisioningArtifact'
  { -- | One or more associations, each consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
    serviceActionAssociations :: Lude.NonEmpty ServiceActionAssociation,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAssociateServiceActionWithProvisioningArtifact' with the minimum fields required to make a request.
--
-- * 'serviceActionAssociations' - One or more associations, each consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
mkBatchAssociateServiceActionWithProvisioningArtifact ::
  -- | 'serviceActionAssociations'
  Lude.NonEmpty ServiceActionAssociation ->
  BatchAssociateServiceActionWithProvisioningArtifact
mkBatchAssociateServiceActionWithProvisioningArtifact
  pServiceActionAssociations_ =
    BatchAssociateServiceActionWithProvisioningArtifact'
      { serviceActionAssociations =
          pServiceActionAssociations_,
        acceptLanguage = Lude.Nothing
      }

-- | One or more associations, each consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
--
-- /Note:/ Consider using 'serviceActionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
basawpaServiceActionAssociations :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifact (Lude.NonEmpty ServiceActionAssociation)
basawpaServiceActionAssociations = Lens.lens (serviceActionAssociations :: BatchAssociateServiceActionWithProvisioningArtifact -> Lude.NonEmpty ServiceActionAssociation) (\s a -> s {serviceActionAssociations = a} :: BatchAssociateServiceActionWithProvisioningArtifact)
{-# DEPRECATED basawpaServiceActionAssociations "Use generic-lens or generic-optics with 'serviceActionAssociations' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
basawpaAcceptLanguage :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifact (Lude.Maybe Lude.Text)
basawpaAcceptLanguage = Lens.lens (acceptLanguage :: BatchAssociateServiceActionWithProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: BatchAssociateServiceActionWithProvisioningArtifact)
{-# DEPRECATED basawpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance
  Lude.AWSRequest
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  type
    Rs BatchAssociateServiceActionWithProvisioningArtifact =
      BatchAssociateServiceActionWithProvisioningArtifactResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchAssociateServiceActionWithProvisioningArtifactResponse'
            Lude.<$> (x Lude..?> "FailedServiceActionAssociations" Lude..!@ Lude.mempty)
              Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.BatchAssociateServiceActionWithProvisioningArtifact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance
  Lude.ToJSON
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toJSON BatchAssociateServiceActionWithProvisioningArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ServiceActionAssociations" Lude..= serviceActionAssociations),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage
          ]
      )

instance
  Lude.ToPath
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchAssociateServiceActionWithProvisioningArtifactResponse' smart constructor.
data BatchAssociateServiceActionWithProvisioningArtifactResponse = BatchAssociateServiceActionWithProvisioningArtifactResponse'
  { -- | An object that contains a list of errors, along with information to help you identify the self-service action.
    failedServiceActionAssociations :: Lude.Maybe [FailedServiceActionAssociation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAssociateServiceActionWithProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- * 'failedServiceActionAssociations' - An object that contains a list of errors, along with information to help you identify the self-service action.
-- * 'responseStatus' - The response status code.
mkBatchAssociateServiceActionWithProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchAssociateServiceActionWithProvisioningArtifactResponse
mkBatchAssociateServiceActionWithProvisioningArtifactResponse
  pResponseStatus_ =
    BatchAssociateServiceActionWithProvisioningArtifactResponse'
      { failedServiceActionAssociations =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | An object that contains a list of errors, along with information to help you identify the self-service action.
--
-- /Note:/ Consider using 'failedServiceActionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
basawparsFailedServiceActionAssociations :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifactResponse (Lude.Maybe [FailedServiceActionAssociation])
basawparsFailedServiceActionAssociations = Lens.lens (failedServiceActionAssociations :: BatchAssociateServiceActionWithProvisioningArtifactResponse -> Lude.Maybe [FailedServiceActionAssociation]) (\s a -> s {failedServiceActionAssociations = a} :: BatchAssociateServiceActionWithProvisioningArtifactResponse)
{-# DEPRECATED basawparsFailedServiceActionAssociations "Use generic-lens or generic-optics with 'failedServiceActionAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
basawparsResponseStatus :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifactResponse Lude.Int
basawparsResponseStatus = Lens.lens (responseStatus :: BatchAssociateServiceActionWithProvisioningArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchAssociateServiceActionWithProvisioningArtifactResponse)
{-# DEPRECATED basawparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

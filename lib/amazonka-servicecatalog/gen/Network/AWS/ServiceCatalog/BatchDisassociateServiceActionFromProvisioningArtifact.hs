{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a batch of self-service actions from the specified provisioning artifact.
module Network.AWS.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
  ( -- * Creating a request
    BatchDisassociateServiceActionFromProvisioningArtifact (..),
    mkBatchDisassociateServiceActionFromProvisioningArtifact,

    -- ** Request lenses
    bdsafpaServiceActionAssociations,
    bdsafpaAcceptLanguage,

    -- * Destructuring the response
    BatchDisassociateServiceActionFromProvisioningArtifactResponse (..),
    mkBatchDisassociateServiceActionFromProvisioningArtifactResponse,

    -- ** Response lenses
    bdsafparsFailedServiceActionAssociations,
    bdsafparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkBatchDisassociateServiceActionFromProvisioningArtifact' smart constructor.
data BatchDisassociateServiceActionFromProvisioningArtifact = BatchDisassociateServiceActionFromProvisioningArtifact'
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

-- | Creates a value of 'BatchDisassociateServiceActionFromProvisioningArtifact' with the minimum fields required to make a request.
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
mkBatchDisassociateServiceActionFromProvisioningArtifact ::
  -- | 'serviceActionAssociations'
  Lude.NonEmpty ServiceActionAssociation ->
  BatchDisassociateServiceActionFromProvisioningArtifact
mkBatchDisassociateServiceActionFromProvisioningArtifact
  pServiceActionAssociations_ =
    BatchDisassociateServiceActionFromProvisioningArtifact'
      { serviceActionAssociations =
          pServiceActionAssociations_,
        acceptLanguage = Lude.Nothing
      }

-- | One or more associations, each consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
--
-- /Note:/ Consider using 'serviceActionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsafpaServiceActionAssociations :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifact (Lude.NonEmpty ServiceActionAssociation)
bdsafpaServiceActionAssociations = Lens.lens (serviceActionAssociations :: BatchDisassociateServiceActionFromProvisioningArtifact -> Lude.NonEmpty ServiceActionAssociation) (\s a -> s {serviceActionAssociations = a} :: BatchDisassociateServiceActionFromProvisioningArtifact)
{-# DEPRECATED bdsafpaServiceActionAssociations "Use generic-lens or generic-optics with 'serviceActionAssociations' instead." #-}

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
bdsafpaAcceptLanguage :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifact (Lude.Maybe Lude.Text)
bdsafpaAcceptLanguage = Lens.lens (acceptLanguage :: BatchDisassociateServiceActionFromProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: BatchDisassociateServiceActionFromProvisioningArtifact)
{-# DEPRECATED bdsafpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance
  Lude.AWSRequest
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  type
    Rs BatchDisassociateServiceActionFromProvisioningArtifact =
      BatchDisassociateServiceActionFromProvisioningArtifactResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDisassociateServiceActionFromProvisioningArtifactResponse'
            Lude.<$> (x Lude..?> "FailedServiceActionAssociations" Lude..!@ Lude.mempty)
              Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.BatchDisassociateServiceActionFromProvisioningArtifact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance
  Lude.ToJSON
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toJSON BatchDisassociateServiceActionFromProvisioningArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ServiceActionAssociations" Lude..= serviceActionAssociations),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage
          ]
      )

instance
  Lude.ToPath
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDisassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
data BatchDisassociateServiceActionFromProvisioningArtifactResponse = BatchDisassociateServiceActionFromProvisioningArtifactResponse'
  { -- | An object that contains a list of errors, along with information to help you identify the self-service action.
    failedServiceActionAssociations :: Lude.Maybe [FailedServiceActionAssociation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDisassociateServiceActionFromProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- * 'failedServiceActionAssociations' - An object that contains a list of errors, along with information to help you identify the self-service action.
-- * 'responseStatus' - The response status code.
mkBatchDisassociateServiceActionFromProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDisassociateServiceActionFromProvisioningArtifactResponse
mkBatchDisassociateServiceActionFromProvisioningArtifactResponse
  pResponseStatus_ =
    BatchDisassociateServiceActionFromProvisioningArtifactResponse'
      { failedServiceActionAssociations =
          Lude.Nothing,
        responseStatus =
          pResponseStatus_
      }

-- | An object that contains a list of errors, along with information to help you identify the self-service action.
--
-- /Note:/ Consider using 'failedServiceActionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsafparsFailedServiceActionAssociations :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifactResponse (Lude.Maybe [FailedServiceActionAssociation])
bdsafparsFailedServiceActionAssociations = Lens.lens (failedServiceActionAssociations :: BatchDisassociateServiceActionFromProvisioningArtifactResponse -> Lude.Maybe [FailedServiceActionAssociation]) (\s a -> s {failedServiceActionAssociations = a} :: BatchDisassociateServiceActionFromProvisioningArtifactResponse)
{-# DEPRECATED bdsafparsFailedServiceActionAssociations "Use generic-lens or generic-optics with 'failedServiceActionAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsafparsResponseStatus :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifactResponse Lude.Int
bdsafparsResponseStatus = Lens.lens (responseStatus :: BatchDisassociateServiceActionFromProvisioningArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDisassociateServiceActionFromProvisioningArtifactResponse)
{-# DEPRECATED bdsafparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

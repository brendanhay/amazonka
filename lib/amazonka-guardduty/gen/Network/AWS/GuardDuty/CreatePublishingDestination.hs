{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreatePublishingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a publishing destination to export findings to. The resource to export findings to must exist before you use this operation.
module Network.AWS.GuardDuty.CreatePublishingDestination
  ( -- * Creating a request
    CreatePublishingDestination (..),
    mkCreatePublishingDestination,

    -- ** Request lenses
    cpdClientToken,
    cpdDestinationType,
    cpdDetectorId,
    cpdDestinationProperties,

    -- * Destructuring the response
    CreatePublishingDestinationResponse (..),
    mkCreatePublishingDestinationResponse,

    -- ** Response lenses
    cpdrsDestinationId,
    cpdrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePublishingDestination' smart constructor.
data CreatePublishingDestination = CreatePublishingDestination'
  { -- | The idempotency token for the request.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
    destinationType :: DestinationType,
    -- | The ID of the GuardDuty detector associated with the publishing destination.
    detectorId :: Lude.Text,
    -- | The properties of the publishing destination, including the ARNs for the destination and the KMS key used for encryption.
    destinationProperties :: DestinationProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePublishingDestination' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token for the request.
-- * 'destinationType' - The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
-- * 'detectorId' - The ID of the GuardDuty detector associated with the publishing destination.
-- * 'destinationProperties' - The properties of the publishing destination, including the ARNs for the destination and the KMS key used for encryption.
mkCreatePublishingDestination ::
  -- | 'destinationType'
  DestinationType ->
  -- | 'detectorId'
  Lude.Text ->
  -- | 'destinationProperties'
  DestinationProperties ->
  CreatePublishingDestination
mkCreatePublishingDestination
  pDestinationType_
  pDetectorId_
  pDestinationProperties_ =
    CreatePublishingDestination'
      { clientToken = Lude.Nothing,
        destinationType = pDestinationType_,
        detectorId = pDetectorId_,
        destinationProperties = pDestinationProperties_
      }

-- | The idempotency token for the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdClientToken :: Lens.Lens' CreatePublishingDestination (Lude.Maybe Lude.Text)
cpdClientToken = Lens.lens (clientToken :: CreatePublishingDestination -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreatePublishingDestination)
{-# DEPRECATED cpdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDestinationType :: Lens.Lens' CreatePublishingDestination DestinationType
cpdDestinationType = Lens.lens (destinationType :: CreatePublishingDestination -> DestinationType) (\s a -> s {destinationType = a} :: CreatePublishingDestination)
{-# DEPRECATED cpdDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | The ID of the GuardDuty detector associated with the publishing destination.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDetectorId :: Lens.Lens' CreatePublishingDestination Lude.Text
cpdDetectorId = Lens.lens (detectorId :: CreatePublishingDestination -> Lude.Text) (\s a -> s {detectorId = a} :: CreatePublishingDestination)
{-# DEPRECATED cpdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The properties of the publishing destination, including the ARNs for the destination and the KMS key used for encryption.
--
-- /Note:/ Consider using 'destinationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDestinationProperties :: Lens.Lens' CreatePublishingDestination DestinationProperties
cpdDestinationProperties = Lens.lens (destinationProperties :: CreatePublishingDestination -> DestinationProperties) (\s a -> s {destinationProperties = a} :: CreatePublishingDestination)
{-# DEPRECATED cpdDestinationProperties "Use generic-lens or generic-optics with 'destinationProperties' instead." #-}

instance Lude.AWSRequest CreatePublishingDestination where
  type
    Rs CreatePublishingDestination =
      CreatePublishingDestinationResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePublishingDestinationResponse'
            Lude.<$> (x Lude..:> "destinationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePublishingDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePublishingDestination where
  toJSON CreatePublishingDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientToken" Lude..=) Lude.<$> clientToken,
            Lude.Just ("destinationType" Lude..= destinationType),
            Lude.Just ("destinationProperties" Lude..= destinationProperties)
          ]
      )

instance Lude.ToPath CreatePublishingDestination where
  toPath CreatePublishingDestination' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/publishingDestination"]

instance Lude.ToQuery CreatePublishingDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePublishingDestinationResponse' smart constructor.
data CreatePublishingDestinationResponse = CreatePublishingDestinationResponse'
  { -- | The ID of the publishing destination that is created.
    destinationId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- * 'destinationId' - The ID of the publishing destination that is created.
-- * 'responseStatus' - The response status code.
mkCreatePublishingDestinationResponse ::
  -- | 'destinationId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreatePublishingDestinationResponse
mkCreatePublishingDestinationResponse
  pDestinationId_
  pResponseStatus_ =
    CreatePublishingDestinationResponse'
      { destinationId =
          pDestinationId_,
        responseStatus = pResponseStatus_
      }

-- | The ID of the publishing destination that is created.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrsDestinationId :: Lens.Lens' CreatePublishingDestinationResponse Lude.Text
cpdrsDestinationId = Lens.lens (destinationId :: CreatePublishingDestinationResponse -> Lude.Text) (\s a -> s {destinationId = a} :: CreatePublishingDestinationResponse)
{-# DEPRECATED cpdrsDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrsResponseStatus :: Lens.Lens' CreatePublishingDestinationResponse Lude.Int
cpdrsResponseStatus = Lens.lens (responseStatus :: CreatePublishingDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePublishingDestinationResponse)
{-# DEPRECATED cpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

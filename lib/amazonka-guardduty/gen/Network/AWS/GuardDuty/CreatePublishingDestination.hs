{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cpdDetectorId,
    cpdDestinationType,
    cpdDestinationProperties,

    -- * Destructuring the response
    CreatePublishingDestinationResponse (..),
    mkCreatePublishingDestinationResponse,

    -- ** Response lenses
    cpdrsResponseStatus,
    cpdrsDestinationId,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePublishingDestination' smart constructor.
data CreatePublishingDestination = CreatePublishingDestination'
  { clientToken ::
      Lude.Maybe Lude.Text,
    detectorId :: Lude.Text,
    destinationType :: DestinationType,
    destinationProperties ::
      DestinationProperties
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePublishingDestination' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token for the request.
-- * 'destinationProperties' - The properties of the publishing destination, including the ARNs for the destination and the KMS key used for encryption.
-- * 'destinationType' - The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
-- * 'detectorId' - The ID of the GuardDuty detector associated with the publishing destination.
mkCreatePublishingDestination ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'destinationType'
  DestinationType ->
  -- | 'destinationProperties'
  DestinationProperties ->
  CreatePublishingDestination
mkCreatePublishingDestination
  pDetectorId_
  pDestinationType_
  pDestinationProperties_ =
    CreatePublishingDestination'
      { clientToken = Lude.Nothing,
        detectorId = pDetectorId_,
        destinationType = pDestinationType_,
        destinationProperties = pDestinationProperties_
      }

-- | The idempotency token for the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdClientToken :: Lens.Lens' CreatePublishingDestination (Lude.Maybe Lude.Text)
cpdClientToken = Lens.lens (clientToken :: CreatePublishingDestination -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreatePublishingDestination)
{-# DEPRECATED cpdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the GuardDuty detector associated with the publishing destination.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDetectorId :: Lens.Lens' CreatePublishingDestination Lude.Text
cpdDetectorId = Lens.lens (detectorId :: CreatePublishingDestination -> Lude.Text) (\s a -> s {detectorId = a} :: CreatePublishingDestination)
{-# DEPRECATED cpdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The type of resource for the publishing destination. Currently only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdDestinationType :: Lens.Lens' CreatePublishingDestination DestinationType
cpdDestinationType = Lens.lens (destinationType :: CreatePublishingDestination -> DestinationType) (\s a -> s {destinationType = a} :: CreatePublishingDestination)
{-# DEPRECATED cpdDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

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
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "destinationId")
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
  { responseStatus ::
      Lude.Int,
    destinationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePublishingDestinationResponse' with the minimum fields required to make a request.
--
-- * 'destinationId' - The ID of the publishing destination that is created.
-- * 'responseStatus' - The response status code.
mkCreatePublishingDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'destinationId'
  Lude.Text ->
  CreatePublishingDestinationResponse
mkCreatePublishingDestinationResponse
  pResponseStatus_
  pDestinationId_ =
    CreatePublishingDestinationResponse'
      { responseStatus =
          pResponseStatus_,
        destinationId = pDestinationId_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrsResponseStatus :: Lens.Lens' CreatePublishingDestinationResponse Lude.Int
cpdrsResponseStatus = Lens.lens (responseStatus :: CreatePublishingDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePublishingDestinationResponse)
{-# DEPRECATED cpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the publishing destination that is created.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdrsDestinationId :: Lens.Lens' CreatePublishingDestinationResponse Lude.Text
cpdrsDestinationId = Lens.lens (destinationId :: CreatePublishingDestinationResponse -> Lude.Text) (\s a -> s {destinationId = a} :: CreatePublishingDestinationResponse)
{-# DEPRECATED cpdrsDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

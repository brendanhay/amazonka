{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Modifies the certificate used by the client.
-- This action can potentially start a workflow to install the new certificate on the client's HSMs.
module Network.AWS.CloudHSM.ModifyLunaClient
  ( -- * Creating a request
    ModifyLunaClient (..),
    mkModifyLunaClient,

    -- ** Request lenses
    mlcClientArn,
    mlcCertificate,

    -- * Destructuring the response
    ModifyLunaClientResponse (..),
    mkModifyLunaClientResponse,

    -- ** Response lenses
    mlcrrsClientArn,
    mlcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyLunaClient' smart constructor.
data ModifyLunaClient = ModifyLunaClient'
  { -- | The ARN of the client.
    clientArn :: Types.ClientArn,
    -- | The new certificate for the client.
    certificate :: Types.Certificate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLunaClient' value with any optional fields omitted.
mkModifyLunaClient ::
  -- | 'clientArn'
  Types.ClientArn ->
  -- | 'certificate'
  Types.Certificate ->
  ModifyLunaClient
mkModifyLunaClient clientArn certificate =
  ModifyLunaClient' {clientArn, certificate}

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcClientArn :: Lens.Lens' ModifyLunaClient Types.ClientArn
mlcClientArn = Lens.field @"clientArn"
{-# DEPRECATED mlcClientArn "Use generic-lens or generic-optics with 'clientArn' instead." #-}

-- | The new certificate for the client.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcCertificate :: Lens.Lens' ModifyLunaClient Types.Certificate
mlcCertificate = Lens.field @"certificate"
{-# DEPRECATED mlcCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

instance Core.FromJSON ModifyLunaClient where
  toJSON ModifyLunaClient {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientArn" Core..= clientArn),
            Core.Just ("Certificate" Core..= certificate)
          ]
      )

instance Core.AWSRequest ModifyLunaClient where
  type Rs ModifyLunaClient = ModifyLunaClientResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CloudHsmFrontendService.ModifyLunaClient")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyLunaClientResponse'
            Core.<$> (x Core..:? "ClientArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyLunaClientResponse' smart constructor.
data ModifyLunaClientResponse = ModifyLunaClientResponse'
  { -- | The ARN of the client.
    clientArn :: Core.Maybe Types.ClientArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLunaClientResponse' value with any optional fields omitted.
mkModifyLunaClientResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyLunaClientResponse
mkModifyLunaClientResponse responseStatus =
  ModifyLunaClientResponse'
    { clientArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcrrsClientArn :: Lens.Lens' ModifyLunaClientResponse (Core.Maybe Types.ClientArn)
mlcrrsClientArn = Lens.field @"clientArn"
{-# DEPRECATED mlcrrsClientArn "Use generic-lens or generic-optics with 'clientArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlcrrsResponseStatus :: Lens.Lens' ModifyLunaClientResponse Core.Int
mlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

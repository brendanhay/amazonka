{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Creates an HSM client.
module Network.AWS.CloudHSM.CreateLunaClient
  ( -- * Creating a request
    CreateLunaClient (..),
    mkCreateLunaClient,

    -- ** Request lenses
    clcCertificate,
    clcLabel,

    -- * Destructuring the response
    CreateLunaClientResponse (..),
    mkCreateLunaClientResponse,

    -- ** Response lenses
    clcrrsClientArn,
    clcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'CreateLunaClient' action.
--
-- /See:/ 'mkCreateLunaClient' smart constructor.
data CreateLunaClient = CreateLunaClient'
  { -- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on the HSMs used by this client.
    certificate :: Types.Certificate,
    -- | The label for the client.
    label :: Core.Maybe Types.ClientLabel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLunaClient' value with any optional fields omitted.
mkCreateLunaClient ::
  -- | 'certificate'
  Types.Certificate ->
  CreateLunaClient
mkCreateLunaClient certificate =
  CreateLunaClient' {certificate, label = Core.Nothing}

-- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on the HSMs used by this client.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcCertificate :: Lens.Lens' CreateLunaClient Types.Certificate
clcCertificate = Lens.field @"certificate"
{-# DEPRECATED clcCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The label for the client.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcLabel :: Lens.Lens' CreateLunaClient (Core.Maybe Types.ClientLabel)
clcLabel = Lens.field @"label"
{-# DEPRECATED clcLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Core.FromJSON CreateLunaClient where
  toJSON CreateLunaClient {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Certificate" Core..= certificate),
            ("Label" Core..=) Core.<$> label
          ]
      )

instance Core.AWSRequest CreateLunaClient where
  type Rs CreateLunaClient = CreateLunaClientResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CloudHsmFrontendService.CreateLunaClient")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLunaClientResponse'
            Core.<$> (x Core..:? "ClientArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of the 'CreateLunaClient' action.
--
-- /See:/ 'mkCreateLunaClientResponse' smart constructor.
data CreateLunaClientResponse = CreateLunaClientResponse'
  { -- | The ARN of the client.
    clientArn :: Core.Maybe Types.ClientArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLunaClientResponse' value with any optional fields omitted.
mkCreateLunaClientResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLunaClientResponse
mkCreateLunaClientResponse responseStatus =
  CreateLunaClientResponse'
    { clientArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcrrsClientArn :: Lens.Lens' CreateLunaClientResponse (Core.Maybe Types.ClientArn)
clcrrsClientArn = Lens.field @"clientArn"
{-# DEPRECATED clcrrsClientArn "Use generic-lens or generic-optics with 'clientArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clcrrsResponseStatus :: Lens.Lens' CreateLunaClientResponse Core.Int
clcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DisassociateHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes health-based detection from the Shield Advanced protection for a resource. Shield Advanced health-based detection uses the health of your AWS resource to improve responsiveness and accuracy in attack detection and mitigation.
--
-- You define the health check in Route 53 and then associate or disassociate it with your Shield Advanced protection. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection> in the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
module Network.AWS.Shield.DisassociateHealthCheck
  ( -- * Creating a request
    DisassociateHealthCheck (..),
    mkDisassociateHealthCheck,

    -- ** Request lenses
    dhcHealthCheckARN,
    dhcProtectionId,

    -- * Destructuring the response
    DisassociateHealthCheckResponse (..),
    mkDisassociateHealthCheckResponse,

    -- ** Response lenses
    dhcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDisassociateHealthCheck' smart constructor.
data DisassociateHealthCheck = DisassociateHealthCheck'
  { -- | The Amazon Resource Name (ARN) of the health check that is associated with the protection.
    healthCheckARN :: Lude.Text,
    -- | The unique identifier (ID) for the 'Protection' object to remove the health check association from.
    protectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateHealthCheck' with the minimum fields required to make a request.
--
-- * 'healthCheckARN' - The Amazon Resource Name (ARN) of the health check that is associated with the protection.
-- * 'protectionId' - The unique identifier (ID) for the 'Protection' object to remove the health check association from.
mkDisassociateHealthCheck ::
  -- | 'healthCheckARN'
  Lude.Text ->
  -- | 'protectionId'
  Lude.Text ->
  DisassociateHealthCheck
mkDisassociateHealthCheck pHealthCheckARN_ pProtectionId_ =
  DisassociateHealthCheck'
    { healthCheckARN = pHealthCheckARN_,
      protectionId = pProtectionId_
    }

-- | The Amazon Resource Name (ARN) of the health check that is associated with the protection.
--
-- /Note:/ Consider using 'healthCheckARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHealthCheckARN :: Lens.Lens' DisassociateHealthCheck Lude.Text
dhcHealthCheckARN = Lens.lens (healthCheckARN :: DisassociateHealthCheck -> Lude.Text) (\s a -> s {healthCheckARN = a} :: DisassociateHealthCheck)
{-# DEPRECATED dhcHealthCheckARN "Use generic-lens or generic-optics with 'healthCheckARN' instead." #-}

-- | The unique identifier (ID) for the 'Protection' object to remove the health check association from.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcProtectionId :: Lens.Lens' DisassociateHealthCheck Lude.Text
dhcProtectionId = Lens.lens (protectionId :: DisassociateHealthCheck -> Lude.Text) (\s a -> s {protectionId = a} :: DisassociateHealthCheck)
{-# DEPRECATED dhcProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

instance Lude.AWSRequest DisassociateHealthCheck where
  type Rs DisassociateHealthCheck = DisassociateHealthCheckResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateHealthCheckResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateHealthCheck where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DisassociateHealthCheck" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateHealthCheck where
  toJSON DisassociateHealthCheck' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("HealthCheckArn" Lude..= healthCheckARN),
            Lude.Just ("ProtectionId" Lude..= protectionId)
          ]
      )

instance Lude.ToPath DisassociateHealthCheck where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateHealthCheck where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateHealthCheckResponse' smart constructor.
newtype DisassociateHealthCheckResponse = DisassociateHealthCheckResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateHealthCheckResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateHealthCheckResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateHealthCheckResponse
mkDisassociateHealthCheckResponse pResponseStatus_ =
  DisassociateHealthCheckResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrsResponseStatus :: Lens.Lens' DisassociateHealthCheckResponse Lude.Int
dhcrsResponseStatus = Lens.lens (responseStatus :: DisassociateHealthCheckResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateHealthCheckResponse)
{-# DEPRECATED dhcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

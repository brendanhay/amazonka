{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds health-based detection to the Shield Advanced protection for a resource. Shield Advanced health-based detection uses the health of your AWS resource to improve responsiveness and accuracy in attack detection and mitigation.
--
-- You define the health check in Route 53 and then associate it with your Shield Advanced protection. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection> in the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
module Network.AWS.Shield.AssociateHealthCheck
  ( -- * Creating a request
    AssociateHealthCheck (..),
    mkAssociateHealthCheck,

    -- ** Request lenses
    ahcHealthCheckARN,
    ahcProtectionId,

    -- * Destructuring the response
    AssociateHealthCheckResponse (..),
    mkAssociateHealthCheckResponse,

    -- ** Response lenses
    ahcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkAssociateHealthCheck' smart constructor.
data AssociateHealthCheck = AssociateHealthCheck'
  { -- | The Amazon Resource Name (ARN) of the health check to associate with the protection.
    healthCheckARN :: Lude.Text,
    -- | The unique identifier (ID) for the 'Protection' object to add the health check association to.
    protectionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateHealthCheck' with the minimum fields required to make a request.
--
-- * 'healthCheckARN' - The Amazon Resource Name (ARN) of the health check to associate with the protection.
-- * 'protectionId' - The unique identifier (ID) for the 'Protection' object to add the health check association to.
mkAssociateHealthCheck ::
  -- | 'healthCheckARN'
  Lude.Text ->
  -- | 'protectionId'
  Lude.Text ->
  AssociateHealthCheck
mkAssociateHealthCheck pHealthCheckARN_ pProtectionId_ =
  AssociateHealthCheck'
    { healthCheckARN = pHealthCheckARN_,
      protectionId = pProtectionId_
    }

-- | The Amazon Resource Name (ARN) of the health check to associate with the protection.
--
-- /Note:/ Consider using 'healthCheckARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcHealthCheckARN :: Lens.Lens' AssociateHealthCheck Lude.Text
ahcHealthCheckARN = Lens.lens (healthCheckARN :: AssociateHealthCheck -> Lude.Text) (\s a -> s {healthCheckARN = a} :: AssociateHealthCheck)
{-# DEPRECATED ahcHealthCheckARN "Use generic-lens or generic-optics with 'healthCheckARN' instead." #-}

-- | The unique identifier (ID) for the 'Protection' object to add the health check association to.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcProtectionId :: Lens.Lens' AssociateHealthCheck Lude.Text
ahcProtectionId = Lens.lens (protectionId :: AssociateHealthCheck -> Lude.Text) (\s a -> s {protectionId = a} :: AssociateHealthCheck)
{-# DEPRECATED ahcProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

instance Lude.AWSRequest AssociateHealthCheck where
  type Rs AssociateHealthCheck = AssociateHealthCheckResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateHealthCheckResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateHealthCheck where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.AssociateHealthCheck" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateHealthCheck where
  toJSON AssociateHealthCheck' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("HealthCheckArn" Lude..= healthCheckARN),
            Lude.Just ("ProtectionId" Lude..= protectionId)
          ]
      )

instance Lude.ToPath AssociateHealthCheck where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateHealthCheck where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateHealthCheckResponse' smart constructor.
newtype AssociateHealthCheckResponse = AssociateHealthCheckResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateHealthCheckResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateHealthCheckResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateHealthCheckResponse
mkAssociateHealthCheckResponse pResponseStatus_ =
  AssociateHealthCheckResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahcrsResponseStatus :: Lens.Lens' AssociateHealthCheckResponse Lude.Int
ahcrsResponseStatus = Lens.lens (responseStatus :: AssociateHealthCheckResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateHealthCheckResponse)
{-# DEPRECATED ahcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the automatic tape creation policy of a gateway. Use this to update the policy with a new set of automatic tape creation rules. This is only supported for tape gateways.
--
-- By default, there is no automatic tape creation policy.
module Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
  ( -- * Creating a request
    UpdateAutomaticTapeCreationPolicy (..),
    mkUpdateAutomaticTapeCreationPolicy,

    -- ** Request lenses
    uatcpGatewayARN,
    uatcpAutomaticTapeCreationRules,

    -- * Destructuring the response
    UpdateAutomaticTapeCreationPolicyResponse (..),
    mkUpdateAutomaticTapeCreationPolicyResponse,

    -- ** Response lenses
    uatcprsGatewayARN,
    uatcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkUpdateAutomaticTapeCreationPolicy' smart constructor.
data UpdateAutomaticTapeCreationPolicy = UpdateAutomaticTapeCreationPolicy'
  { gatewayARN :: Lude.Text,
    -- | An automatic tape creation policy consists of a list of automatic tape creation rules. The rules determine when and how to automatically create new tapes.
    automaticTapeCreationRules :: Lude.NonEmpty AutomaticTapeCreationRule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAutomaticTapeCreationPolicy' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'automaticTapeCreationRules' - An automatic tape creation policy consists of a list of automatic tape creation rules. The rules determine when and how to automatically create new tapes.
mkUpdateAutomaticTapeCreationPolicy ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'automaticTapeCreationRules'
  Lude.NonEmpty AutomaticTapeCreationRule ->
  UpdateAutomaticTapeCreationPolicy
mkUpdateAutomaticTapeCreationPolicy
  pGatewayARN_
  pAutomaticTapeCreationRules_ =
    UpdateAutomaticTapeCreationPolicy'
      { gatewayARN = pGatewayARN_,
        automaticTapeCreationRules = pAutomaticTapeCreationRules_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatcpGatewayARN :: Lens.Lens' UpdateAutomaticTapeCreationPolicy Lude.Text
uatcpGatewayARN = Lens.lens (gatewayARN :: UpdateAutomaticTapeCreationPolicy -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateAutomaticTapeCreationPolicy)
{-# DEPRECATED uatcpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An automatic tape creation policy consists of a list of automatic tape creation rules. The rules determine when and how to automatically create new tapes.
--
-- /Note:/ Consider using 'automaticTapeCreationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatcpAutomaticTapeCreationRules :: Lens.Lens' UpdateAutomaticTapeCreationPolicy (Lude.NonEmpty AutomaticTapeCreationRule)
uatcpAutomaticTapeCreationRules = Lens.lens (automaticTapeCreationRules :: UpdateAutomaticTapeCreationPolicy -> Lude.NonEmpty AutomaticTapeCreationRule) (\s a -> s {automaticTapeCreationRules = a} :: UpdateAutomaticTapeCreationPolicy)
{-# DEPRECATED uatcpAutomaticTapeCreationRules "Use generic-lens or generic-optics with 'automaticTapeCreationRules' instead." #-}

instance Lude.AWSRequest UpdateAutomaticTapeCreationPolicy where
  type
    Rs UpdateAutomaticTapeCreationPolicy =
      UpdateAutomaticTapeCreationPolicyResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAutomaticTapeCreationPolicyResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAutomaticTapeCreationPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateAutomaticTapeCreationPolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAutomaticTapeCreationPolicy where
  toJSON UpdateAutomaticTapeCreationPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just
              ("AutomaticTapeCreationRules" Lude..= automaticTapeCreationRules)
          ]
      )

instance Lude.ToPath UpdateAutomaticTapeCreationPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAutomaticTapeCreationPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAutomaticTapeCreationPolicyResponse' smart constructor.
data UpdateAutomaticTapeCreationPolicyResponse = UpdateAutomaticTapeCreationPolicyResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAutomaticTapeCreationPolicyResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'responseStatus' - The response status code.
mkUpdateAutomaticTapeCreationPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAutomaticTapeCreationPolicyResponse
mkUpdateAutomaticTapeCreationPolicyResponse pResponseStatus_ =
  UpdateAutomaticTapeCreationPolicyResponse'
    { gatewayARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatcprsGatewayARN :: Lens.Lens' UpdateAutomaticTapeCreationPolicyResponse (Lude.Maybe Lude.Text)
uatcprsGatewayARN = Lens.lens (gatewayARN :: UpdateAutomaticTapeCreationPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateAutomaticTapeCreationPolicyResponse)
{-# DEPRECATED uatcprsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatcprsResponseStatus :: Lens.Lens' UpdateAutomaticTapeCreationPolicyResponse Lude.Int
uatcprsResponseStatus = Lens.lens (responseStatus :: UpdateAutomaticTapeCreationPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAutomaticTapeCreationPolicyResponse)
{-# DEPRECATED uatcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

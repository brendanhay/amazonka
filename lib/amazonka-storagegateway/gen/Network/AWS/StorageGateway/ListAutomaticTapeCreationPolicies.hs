{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the automatic tape creation policies for a gateway. If there are no automatic tape creation policies for the gateway, it returns an empty list.
--
-- This operation is only supported for tape gateways.
module Network.AWS.StorageGateway.ListAutomaticTapeCreationPolicies
  ( -- * Creating a request
    ListAutomaticTapeCreationPolicies (..),
    mkListAutomaticTapeCreationPolicies,

    -- ** Request lenses
    latcpGatewayARN,

    -- * Destructuring the response
    ListAutomaticTapeCreationPoliciesResponse (..),
    mkListAutomaticTapeCreationPoliciesResponse,

    -- ** Response lenses
    latcprsAutomaticTapeCreationPolicyInfos,
    latcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkListAutomaticTapeCreationPolicies' smart constructor.
newtype ListAutomaticTapeCreationPolicies = ListAutomaticTapeCreationPolicies'
  { gatewayARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAutomaticTapeCreationPolicies' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkListAutomaticTapeCreationPolicies ::
  ListAutomaticTapeCreationPolicies
mkListAutomaticTapeCreationPolicies =
  ListAutomaticTapeCreationPolicies' {gatewayARN = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latcpGatewayARN :: Lens.Lens' ListAutomaticTapeCreationPolicies (Lude.Maybe Lude.Text)
latcpGatewayARN = Lens.lens (gatewayARN :: ListAutomaticTapeCreationPolicies -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ListAutomaticTapeCreationPolicies)
{-# DEPRECATED latcpGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest ListAutomaticTapeCreationPolicies where
  type
    Rs ListAutomaticTapeCreationPolicies =
      ListAutomaticTapeCreationPoliciesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAutomaticTapeCreationPoliciesResponse'
            Lude.<$> ( x Lude..?> "AutomaticTapeCreationPolicyInfos"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAutomaticTapeCreationPolicies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.ListAutomaticTapeCreationPolicies" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAutomaticTapeCreationPolicies where
  toJSON ListAutomaticTapeCreationPolicies' {..} =
    Lude.object
      (Lude.catMaybes [("GatewayARN" Lude..=) Lude.<$> gatewayARN])

instance Lude.ToPath ListAutomaticTapeCreationPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAutomaticTapeCreationPolicies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAutomaticTapeCreationPoliciesResponse' smart constructor.
data ListAutomaticTapeCreationPoliciesResponse = ListAutomaticTapeCreationPoliciesResponse'
  { -- | Gets a listing of information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
    automaticTapeCreationPolicyInfos :: Lude.Maybe [AutomaticTapeCreationPolicyInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAutomaticTapeCreationPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'automaticTapeCreationPolicyInfos' - Gets a listing of information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
-- * 'responseStatus' - The response status code.
mkListAutomaticTapeCreationPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAutomaticTapeCreationPoliciesResponse
mkListAutomaticTapeCreationPoliciesResponse pResponseStatus_ =
  ListAutomaticTapeCreationPoliciesResponse'
    { automaticTapeCreationPolicyInfos =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Gets a listing of information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
--
-- /Note:/ Consider using 'automaticTapeCreationPolicyInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latcprsAutomaticTapeCreationPolicyInfos :: Lens.Lens' ListAutomaticTapeCreationPoliciesResponse (Lude.Maybe [AutomaticTapeCreationPolicyInfo])
latcprsAutomaticTapeCreationPolicyInfos = Lens.lens (automaticTapeCreationPolicyInfos :: ListAutomaticTapeCreationPoliciesResponse -> Lude.Maybe [AutomaticTapeCreationPolicyInfo]) (\s a -> s {automaticTapeCreationPolicyInfos = a} :: ListAutomaticTapeCreationPoliciesResponse)
{-# DEPRECATED latcprsAutomaticTapeCreationPolicyInfos "Use generic-lens or generic-optics with 'automaticTapeCreationPolicyInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
latcprsResponseStatus :: Lens.Lens' ListAutomaticTapeCreationPoliciesResponse Lude.Int
latcprsResponseStatus = Lens.lens (responseStatus :: ListAutomaticTapeCreationPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAutomaticTapeCreationPoliciesResponse)
{-# DEPRECATED latcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

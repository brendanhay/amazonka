{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListPolicyVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of the specified policy and identifies the default version.
module Network.AWS.IoT.ListPolicyVersions
  ( -- * Creating a request
    ListPolicyVersions (..),
    mkListPolicyVersions,

    -- ** Request lenses
    lpvPolicyName,

    -- * Destructuring the response
    ListPolicyVersionsResponse (..),
    mkListPolicyVersionsResponse,

    -- ** Response lenses
    lpvrsPolicyVersions,
    lpvrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the ListPolicyVersions operation.
--
-- /See:/ 'mkListPolicyVersions' smart constructor.
newtype ListPolicyVersions = ListPolicyVersions'
  { policyName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicyVersions' with the minimum fields required to make a request.
--
-- * 'policyName' - The policy name.
mkListPolicyVersions ::
  -- | 'policyName'
  Lude.Text ->
  ListPolicyVersions
mkListPolicyVersions pPolicyName_ =
  ListPolicyVersions' {policyName = pPolicyName_}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvPolicyName :: Lens.Lens' ListPolicyVersions Lude.Text
lpvPolicyName = Lens.lens (policyName :: ListPolicyVersions -> Lude.Text) (\s a -> s {policyName = a} :: ListPolicyVersions)
{-# DEPRECATED lpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest ListPolicyVersions where
  type Rs ListPolicyVersions = ListPolicyVersionsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPolicyVersionsResponse'
            Lude.<$> (x Lude..?> "policyVersions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPolicyVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPolicyVersions where
  toPath ListPolicyVersions' {..} =
    Lude.mconcat ["/policies/", Lude.toBS policyName, "/version"]

instance Lude.ToQuery ListPolicyVersions where
  toQuery = Lude.const Lude.mempty

-- | The output from the ListPolicyVersions operation.
--
-- /See:/ 'mkListPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { policyVersions ::
      Lude.Maybe [PolicyVersion],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicyVersionsResponse' with the minimum fields required to make a request.
--
-- * 'policyVersions' - The policy versions.
-- * 'responseStatus' - The response status code.
mkListPolicyVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPolicyVersionsResponse
mkListPolicyVersionsResponse pResponseStatus_ =
  ListPolicyVersionsResponse'
    { policyVersions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The policy versions.
--
-- /Note:/ Consider using 'policyVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsPolicyVersions :: Lens.Lens' ListPolicyVersionsResponse (Lude.Maybe [PolicyVersion])
lpvrsPolicyVersions = Lens.lens (policyVersions :: ListPolicyVersionsResponse -> Lude.Maybe [PolicyVersion]) (\s a -> s {policyVersions = a} :: ListPolicyVersionsResponse)
{-# DEPRECATED lpvrsPolicyVersions "Use generic-lens or generic-optics with 'policyVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrsResponseStatus :: Lens.Lens' ListPolicyVersionsResponse Lude.Int
lpvrsResponseStatus = Lens.lens (responseStatus :: ListPolicyVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPolicyVersionsResponse)
{-# DEPRECATED lpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

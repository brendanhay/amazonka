{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeletePolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of the specified policy. You cannot delete the default version of a policy using this API. To delete the default version of a policy, use 'DeletePolicy' . To find out which version of a policy is marked as the default version, use ListPolicyVersions.
module Network.AWS.IoT.DeletePolicyVersion
  ( -- * Creating a request
    DeletePolicyVersion (..),
    mkDeletePolicyVersion,

    -- ** Request lenses
    dpvPolicyName,
    dpvPolicyVersionId,

    -- * Destructuring the response
    DeletePolicyVersionResponse (..),
    mkDeletePolicyVersionResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeletePolicyVersion operation.
--
-- /See:/ 'mkDeletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { -- | The name of the policy.
    policyName :: Lude.Text,
    -- | The policy version ID.
    policyVersionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicyVersion' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy.
-- * 'policyVersionId' - The policy version ID.
mkDeletePolicyVersion ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyVersionId'
  Lude.Text ->
  DeletePolicyVersion
mkDeletePolicyVersion pPolicyName_ pPolicyVersionId_ =
  DeletePolicyVersion'
    { policyName = pPolicyName_,
      policyVersionId = pPolicyVersionId_
    }

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvPolicyName :: Lens.Lens' DeletePolicyVersion Lude.Text
dpvPolicyName = Lens.lens (policyName :: DeletePolicyVersion -> Lude.Text) (\s a -> s {policyName = a} :: DeletePolicyVersion)
{-# DEPRECATED dpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvPolicyVersionId :: Lens.Lens' DeletePolicyVersion Lude.Text
dpvPolicyVersionId = Lens.lens (policyVersionId :: DeletePolicyVersion -> Lude.Text) (\s a -> s {policyVersionId = a} :: DeletePolicyVersion)
{-# DEPRECATED dpvPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

instance Lude.AWSRequest DeletePolicyVersion where
  type Rs DeletePolicyVersion = DeletePolicyVersionResponse
  request = Req.delete ioTService
  response = Res.receiveNull DeletePolicyVersionResponse'

instance Lude.ToHeaders DeletePolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePolicyVersion where
  toPath DeletePolicyVersion' {..} =
    Lude.mconcat
      [ "/policies/",
        Lude.toBS policyName,
        "/version/",
        Lude.toBS policyVersionId
      ]

instance Lude.ToQuery DeletePolicyVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse = DeletePolicyVersionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicyVersionResponse' with the minimum fields required to make a request.
mkDeletePolicyVersionResponse ::
  DeletePolicyVersionResponse
mkDeletePolicyVersionResponse = DeletePolicyVersionResponse'

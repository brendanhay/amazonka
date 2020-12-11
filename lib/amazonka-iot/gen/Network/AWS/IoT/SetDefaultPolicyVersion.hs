{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy's default (operative) version. This action affects all certificates to which the policy is attached. To list the principals the policy is attached to, use the ListPrincipalPolicy API.
module Network.AWS.IoT.SetDefaultPolicyVersion
  ( -- * Creating a request
    SetDefaultPolicyVersion (..),
    mkSetDefaultPolicyVersion,

    -- ** Request lenses
    sdpvPolicyName,
    sdpvPolicyVersionId,

    -- * Destructuring the response
    SetDefaultPolicyVersionResponse (..),
    mkSetDefaultPolicyVersionResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the SetDefaultPolicyVersion operation.
--
-- /See:/ 'mkSetDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { policyName ::
      Lude.Text,
    policyVersionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDefaultPolicyVersion' with the minimum fields required to make a request.
--
-- * 'policyName' - The policy name.
-- * 'policyVersionId' - The policy version ID.
mkSetDefaultPolicyVersion ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyVersionId'
  Lude.Text ->
  SetDefaultPolicyVersion
mkSetDefaultPolicyVersion pPolicyName_ pPolicyVersionId_ =
  SetDefaultPolicyVersion'
    { policyName = pPolicyName_,
      policyVersionId = pPolicyVersionId_
    }

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvPolicyName :: Lens.Lens' SetDefaultPolicyVersion Lude.Text
sdpvPolicyName = Lens.lens (policyName :: SetDefaultPolicyVersion -> Lude.Text) (\s a -> s {policyName = a} :: SetDefaultPolicyVersion)
{-# DEPRECATED sdpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvPolicyVersionId :: Lens.Lens' SetDefaultPolicyVersion Lude.Text
sdpvPolicyVersionId = Lens.lens (policyVersionId :: SetDefaultPolicyVersion -> Lude.Text) (\s a -> s {policyVersionId = a} :: SetDefaultPolicyVersion)
{-# DEPRECATED sdpvPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

instance Lude.AWSRequest SetDefaultPolicyVersion where
  type Rs SetDefaultPolicyVersion = SetDefaultPolicyVersionResponse
  request = Req.patchJSON ioTService
  response = Res.receiveNull SetDefaultPolicyVersionResponse'

instance Lude.ToHeaders SetDefaultPolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SetDefaultPolicyVersion where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath SetDefaultPolicyVersion where
  toPath SetDefaultPolicyVersion' {..} =
    Lude.mconcat
      [ "/policies/",
        Lude.toBS policyName,
        "/version/",
        Lude.toBS policyVersionId
      ]

instance Lude.ToQuery SetDefaultPolicyVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDefaultPolicyVersionResponse' with the minimum fields required to make a request.
mkSetDefaultPolicyVersionResponse ::
  SetDefaultPolicyVersionResponse
mkSetDefaultPolicyVersionResponse =
  SetDefaultPolicyVersionResponse'

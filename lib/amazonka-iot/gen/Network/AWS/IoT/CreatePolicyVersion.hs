{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreatePolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified AWS IoT policy. To update a policy, create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must use 'DeletePolicyVersion' to delete an existing version before you create a new one.
--
-- Optionally, you can set the new version as the policy's default version. The default version is the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
module Network.AWS.IoT.CreatePolicyVersion
  ( -- * Creating a request
    CreatePolicyVersion (..),
    mkCreatePolicyVersion,

    -- ** Request lenses
    cpvSetAsDefault,
    cpvPolicyName,
    cpvPolicyDocument,

    -- * Destructuring the response
    CreatePolicyVersionResponse (..),
    mkCreatePolicyVersionResponse,

    -- ** Response lenses
    cpvrsPolicyDocument,
    cpvrsPolicyVersionId,
    cpvrsPolicyARN,
    cpvrsIsDefaultVersion,
    cpvrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the CreatePolicyVersion operation.
--
-- /See:/ 'mkCreatePolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { setAsDefault ::
      Lude.Maybe Lude.Bool,
    policyName :: Lude.Text,
    policyDocument :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePolicyVersion' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The JSON document that describes the policy. Minimum length of 1. Maximum length of 2048, excluding whitespace.
-- * 'policyName' - The policy name.
-- * 'setAsDefault' - Specifies whether the policy version is set as the default. When this parameter is true, the new policy version becomes the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
mkCreatePolicyVersion ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyDocument'
  Lude.Text ->
  CreatePolicyVersion
mkCreatePolicyVersion pPolicyName_ pPolicyDocument_ =
  CreatePolicyVersion'
    { setAsDefault = Lude.Nothing,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | Specifies whether the policy version is set as the default. When this parameter is true, the new policy version becomes the operative version (that is, the version that is in effect for the certificates to which the policy is attached).
--
-- /Note:/ Consider using 'setAsDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvSetAsDefault :: Lens.Lens' CreatePolicyVersion (Lude.Maybe Lude.Bool)
cpvSetAsDefault = Lens.lens (setAsDefault :: CreatePolicyVersion -> Lude.Maybe Lude.Bool) (\s a -> s {setAsDefault = a} :: CreatePolicyVersion)
{-# DEPRECATED cpvSetAsDefault "Use generic-lens or generic-optics with 'setAsDefault' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPolicyName :: Lens.Lens' CreatePolicyVersion Lude.Text
cpvPolicyName = Lens.lens (policyName :: CreatePolicyVersion -> Lude.Text) (\s a -> s {policyName = a} :: CreatePolicyVersion)
{-# DEPRECATED cpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON document that describes the policy. Minimum length of 1. Maximum length of 2048, excluding whitespace.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPolicyDocument :: Lens.Lens' CreatePolicyVersion Lude.Text
cpvPolicyDocument = Lens.lens (policyDocument :: CreatePolicyVersion -> Lude.Text) (\s a -> s {policyDocument = a} :: CreatePolicyVersion)
{-# DEPRECATED cpvPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

instance Lude.AWSRequest CreatePolicyVersion where
  type Rs CreatePolicyVersion = CreatePolicyVersionResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePolicyVersionResponse'
            Lude.<$> (x Lude..?> "policyDocument")
            Lude.<*> (x Lude..?> "policyVersionId")
            Lude.<*> (x Lude..?> "policyArn")
            Lude.<*> (x Lude..?> "isDefaultVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreatePolicyVersion where
  toJSON CreatePolicyVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("policyDocument" Lude..= policyDocument)]
      )

instance Lude.ToPath CreatePolicyVersion where
  toPath CreatePolicyVersion' {..} =
    Lude.mconcat ["/policies/", Lude.toBS policyName, "/version"]

instance Lude.ToQuery CreatePolicyVersion where
  toQuery CreatePolicyVersion' {..} =
    Lude.mconcat ["setAsDefault" Lude.=: setAsDefault]

-- | The output of the CreatePolicyVersion operation.
--
-- /See:/ 'mkCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { policyDocument ::
      Lude.Maybe Lude.Text,
    policyVersionId ::
      Lude.Maybe Lude.Text,
    policyARN :: Lude.Maybe Lude.Text,
    isDefaultVersion ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'CreatePolicyVersionResponse' with the minimum fields required to make a request.
--
-- * 'isDefaultVersion' - Specifies whether the policy version is the default.
-- * 'policyARN' - The policy ARN.
-- * 'policyDocument' - The JSON document that describes the policy.
-- * 'policyVersionId' - The policy version ID.
-- * 'responseStatus' - The response status code.
mkCreatePolicyVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePolicyVersionResponse
mkCreatePolicyVersionResponse pResponseStatus_ =
  CreatePolicyVersionResponse'
    { policyDocument = Lude.Nothing,
      policyVersionId = Lude.Nothing,
      policyARN = Lude.Nothing,
      isDefaultVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsPolicyDocument :: Lens.Lens' CreatePolicyVersionResponse (Lude.Maybe Lude.Text)
cpvrsPolicyDocument = Lens.lens (policyDocument :: CreatePolicyVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: CreatePolicyVersionResponse)
{-# DEPRECATED cpvrsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsPolicyVersionId :: Lens.Lens' CreatePolicyVersionResponse (Lude.Maybe Lude.Text)
cpvrsPolicyVersionId = Lens.lens (policyVersionId :: CreatePolicyVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyVersionId = a} :: CreatePolicyVersionResponse)
{-# DEPRECATED cpvrsPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsPolicyARN :: Lens.Lens' CreatePolicyVersionResponse (Lude.Maybe Lude.Text)
cpvrsPolicyARN = Lens.lens (policyARN :: CreatePolicyVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: CreatePolicyVersionResponse)
{-# DEPRECATED cpvrsPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | Specifies whether the policy version is the default.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsIsDefaultVersion :: Lens.Lens' CreatePolicyVersionResponse (Lude.Maybe Lude.Bool)
cpvrsIsDefaultVersion = Lens.lens (isDefaultVersion :: CreatePolicyVersionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: CreatePolicyVersionResponse)
{-# DEPRECATED cpvrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsResponseStatus :: Lens.Lens' CreatePolicyVersionResponse Lude.Int
cpvrsResponseStatus = Lens.lens (responseStatus :: CreatePolicyVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePolicyVersionResponse)
{-# DEPRECATED cpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

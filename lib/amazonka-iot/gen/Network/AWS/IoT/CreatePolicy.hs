{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT policy.
--
-- The created policy is the default version for the policy. This operation creates a policy version with a version identifier of __1__ and sets __1__ as the policy's default version.
module Network.AWS.IoT.CreatePolicy
  ( -- * Creating a request
    CreatePolicy (..),
    mkCreatePolicy,

    -- ** Request lenses
    cpTags,
    cpPolicyName,
    cpPolicyDocument,

    -- * Destructuring the response
    CreatePolicyResponse (..),
    mkCreatePolicyResponse,

    -- ** Response lenses
    cprsPolicyName,
    cprsPolicyDocument,
    cprsPolicyVersionId,
    cprsPolicyARN,
    cprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the CreatePolicy operation.
--
-- /See:/ 'mkCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'CreatePolicy' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The JSON document that describes the policy. __policyDocument__ must have a minimum length of 1, with a maximum length of 2048, excluding whitespace.
-- * 'policyName' - The policy name.
-- * 'tags' - Metadata which can be used to manage the policy.
mkCreatePolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyDocument'
  Lude.Text ->
  CreatePolicy
mkCreatePolicy pPolicyName_ pPolicyDocument_ =
  CreatePolicy'
    { tags = Lude.Nothing,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | Metadata which can be used to manage the policy.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreatePolicy (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CreatePolicy -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePolicy)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyName :: Lens.Lens' CreatePolicy Lude.Text
cpPolicyName = Lens.lens (policyName :: CreatePolicy -> Lude.Text) (\s a -> s {policyName = a} :: CreatePolicy)
{-# DEPRECATED cpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON document that describes the policy. __policyDocument__ must have a minimum length of 1, with a maximum length of 2048, excluding whitespace.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyDocument :: Lens.Lens' CreatePolicy Lude.Text
cpPolicyDocument = Lens.lens (policyDocument :: CreatePolicy -> Lude.Text) (\s a -> s {policyDocument = a} :: CreatePolicy)
{-# DEPRECATED cpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

instance Lude.AWSRequest CreatePolicy where
  type Rs CreatePolicy = CreatePolicyResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePolicyResponse'
            Lude.<$> (x Lude..?> "policyName")
            Lude.<*> (x Lude..?> "policyDocument")
            Lude.<*> (x Lude..?> "policyVersionId")
            Lude.<*> (x Lude..?> "policyArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreatePolicy where
  toJSON CreatePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("policyDocument" Lude..= policyDocument)
          ]
      )

instance Lude.ToPath CreatePolicy where
  toPath CreatePolicy' {..} =
    Lude.mconcat ["/policies/", Lude.toBS policyName]

instance Lude.ToQuery CreatePolicy where
  toQuery = Lude.const Lude.mempty

-- | The output from the CreatePolicy operation.
--
-- /See:/ 'mkCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { policyName ::
      Lude.Maybe Lude.Text,
    policyDocument :: Lude.Maybe Lude.Text,
    policyVersionId :: Lude.Maybe Lude.Text,
    policyARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreatePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyARN' - The policy ARN.
-- * 'policyDocument' - The JSON document that describes the policy.
-- * 'policyName' - The policy name.
-- * 'policyVersionId' - The policy version ID.
-- * 'responseStatus' - The response status code.
mkCreatePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePolicyResponse
mkCreatePolicyResponse pResponseStatus_ =
  CreatePolicyResponse'
    { policyName = Lude.Nothing,
      policyDocument = Lude.Nothing,
      policyVersionId = Lude.Nothing,
      policyARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPolicyName :: Lens.Lens' CreatePolicyResponse (Lude.Maybe Lude.Text)
cprsPolicyName = Lens.lens (policyName :: CreatePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPolicyDocument :: Lens.Lens' CreatePolicyResponse (Lude.Maybe Lude.Text)
cprsPolicyDocument = Lens.lens (policyDocument :: CreatePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPolicyVersionId :: Lens.Lens' CreatePolicyResponse (Lude.Maybe Lude.Text)
cprsPolicyVersionId = Lens.lens (policyVersionId :: CreatePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyVersionId = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPolicyARN :: Lens.Lens' CreatePolicyResponse (Lude.Maybe Lude.Text)
cprsPolicyARN = Lens.lens (policyARN :: CreatePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePolicyResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

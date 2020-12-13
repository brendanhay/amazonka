{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified policy with the policy document of the default version.
module Network.AWS.IoT.GetPolicy
  ( -- * Creating a request
    GetPolicy (..),
    mkGetPolicy,

    -- ** Request lenses
    gpPolicyName,

    -- * Destructuring the response
    GetPolicyResponse (..),
    mkGetPolicyResponse,

    -- ** Response lenses
    gprsLastModifiedDate,
    gprsPolicyName,
    gprsPolicyDocument,
    gprsDefaultVersionId,
    gprsPolicyARN,
    gprsCreationDate,
    gprsGenerationId,
    gprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the GetPolicy operation.
--
-- /See:/ 'mkGetPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { -- | The name of the policy.
    policyName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy.
mkGetPolicy ::
  -- | 'policyName'
  Lude.Text ->
  GetPolicy
mkGetPolicy pPolicyName_ = GetPolicy' {policyName = pPolicyName_}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyName :: Lens.Lens' GetPolicy Lude.Text
gpPolicyName = Lens.lens (policyName :: GetPolicy -> Lude.Text) (\s a -> s {policyName = a} :: GetPolicy)
{-# DEPRECATED gpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Lude.<$> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "policyName")
            Lude.<*> (x Lude..?> "policyDocument")
            Lude.<*> (x Lude..?> "defaultVersionId")
            Lude.<*> (x Lude..?> "policyArn")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "generationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPolicy where
  toPath GetPolicy' {..} =
    Lude.mconcat ["/policies/", Lude.toBS policyName]

instance Lude.ToQuery GetPolicy where
  toQuery = Lude.const Lude.mempty

-- | The output from the GetPolicy operation.
--
-- /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | The date the policy was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The policy name.
    policyName :: Lude.Maybe Lude.Text,
    -- | The JSON document that describes the policy.
    policyDocument :: Lude.Maybe Lude.Text,
    -- | The default policy version ID.
    defaultVersionId :: Lude.Maybe Lude.Text,
    -- | The policy ARN.
    policyARN :: Lude.Maybe Lude.Text,
    -- | The date the policy was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The generation ID of the policy.
    generationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - The date the policy was last modified.
-- * 'policyName' - The policy name.
-- * 'policyDocument' - The JSON document that describes the policy.
-- * 'defaultVersionId' - The default policy version ID.
-- * 'policyARN' - The policy ARN.
-- * 'creationDate' - The date the policy was created.
-- * 'generationId' - The generation ID of the policy.
-- * 'responseStatus' - The response status code.
mkGetPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPolicyResponse
mkGetPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { lastModifiedDate = Lude.Nothing,
      policyName = Lude.Nothing,
      policyDocument = Lude.Nothing,
      defaultVersionId = Lude.Nothing,
      policyARN = Lude.Nothing,
      creationDate = Lude.Nothing,
      generationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date the policy was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsLastModifiedDate :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Timestamp)
gprsLastModifiedDate = Lens.lens (lastModifiedDate :: GetPolicyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: GetPolicyResponse)
{-# DEPRECATED gprsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPolicyName :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsPolicyName = Lens.lens (policyName :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: GetPolicyResponse)
{-# DEPRECATED gprsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPolicyDocument :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsPolicyDocument = Lens.lens (policyDocument :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: GetPolicyResponse)
{-# DEPRECATED gprsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The default policy version ID.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsDefaultVersionId :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsDefaultVersionId = Lens.lens (defaultVersionId :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersionId = a} :: GetPolicyResponse)
{-# DEPRECATED gprsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPolicyARN :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsPolicyARN = Lens.lens (policyARN :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: GetPolicyResponse)
{-# DEPRECATED gprsPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The date the policy was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsCreationDate :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Timestamp)
gprsCreationDate = Lens.lens (creationDate :: GetPolicyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: GetPolicyResponse)
{-# DEPRECATED gprsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The generation ID of the policy.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsGenerationId :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsGenerationId = Lens.lens (generationId :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {generationId = a} :: GetPolicyResponse)
{-# DEPRECATED gprsGenerationId "Use generic-lens or generic-optics with 'generationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetPolicyResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPolicyResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

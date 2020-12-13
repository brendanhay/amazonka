{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified policy version.
module Network.AWS.IoT.GetPolicyVersion
  ( -- * Creating a request
    GetPolicyVersion (..),
    mkGetPolicyVersion,

    -- ** Request lenses
    gpvPolicyName,
    gpvPolicyVersionId,

    -- * Destructuring the response
    GetPolicyVersionResponse (..),
    mkGetPolicyVersionResponse,

    -- ** Response lenses
    gpvrsLastModifiedDate,
    gpvrsPolicyName,
    gpvrsPolicyDocument,
    gpvrsPolicyVersionId,
    gpvrsPolicyARN,
    gpvrsCreationDate,
    gpvrsGenerationId,
    gpvrsIsDefaultVersion,
    gpvrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the GetPolicyVersion operation.
--
-- /See:/ 'mkGetPolicyVersion' smart constructor.
data GetPolicyVersion = GetPolicyVersion'
  { -- | The name of the policy.
    policyName :: Lude.Text,
    -- | The policy version ID.
    policyVersionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicyVersion' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy.
-- * 'policyVersionId' - The policy version ID.
mkGetPolicyVersion ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyVersionId'
  Lude.Text ->
  GetPolicyVersion
mkGetPolicyVersion pPolicyName_ pPolicyVersionId_ =
  GetPolicyVersion'
    { policyName = pPolicyName_,
      policyVersionId = pPolicyVersionId_
    }

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvPolicyName :: Lens.Lens' GetPolicyVersion Lude.Text
gpvPolicyName = Lens.lens (policyName :: GetPolicyVersion -> Lude.Text) (\s a -> s {policyName = a} :: GetPolicyVersion)
{-# DEPRECATED gpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvPolicyVersionId :: Lens.Lens' GetPolicyVersion Lude.Text
gpvPolicyVersionId = Lens.lens (policyVersionId :: GetPolicyVersion -> Lude.Text) (\s a -> s {policyVersionId = a} :: GetPolicyVersion)
{-# DEPRECATED gpvPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

instance Lude.AWSRequest GetPolicyVersion where
  type Rs GetPolicyVersion = GetPolicyVersionResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPolicyVersionResponse'
            Lude.<$> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "policyName")
            Lude.<*> (x Lude..?> "policyDocument")
            Lude.<*> (x Lude..?> "policyVersionId")
            Lude.<*> (x Lude..?> "policyArn")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "generationId")
            Lude.<*> (x Lude..?> "isDefaultVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPolicyVersion where
  toPath GetPolicyVersion' {..} =
    Lude.mconcat
      [ "/policies/",
        Lude.toBS policyName,
        "/version/",
        Lude.toBS policyVersionId
      ]

instance Lude.ToQuery GetPolicyVersion where
  toQuery = Lude.const Lude.mempty

-- | The output from the GetPolicyVersion operation.
--
-- /See:/ 'mkGetPolicyVersionResponse' smart constructor.
data GetPolicyVersionResponse = GetPolicyVersionResponse'
  { -- | The date the policy was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The policy name.
    policyName :: Lude.Maybe Lude.Text,
    -- | The JSON document that describes the policy.
    policyDocument :: Lude.Maybe Lude.Text,
    -- | The policy version ID.
    policyVersionId :: Lude.Maybe Lude.Text,
    -- | The policy ARN.
    policyARN :: Lude.Maybe Lude.Text,
    -- | The date the policy was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The generation ID of the policy version.
    generationId :: Lude.Maybe Lude.Text,
    -- | Specifies whether the policy version is the default.
    isDefaultVersion :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicyVersionResponse' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - The date the policy was last modified.
-- * 'policyName' - The policy name.
-- * 'policyDocument' - The JSON document that describes the policy.
-- * 'policyVersionId' - The policy version ID.
-- * 'policyARN' - The policy ARN.
-- * 'creationDate' - The date the policy was created.
-- * 'generationId' - The generation ID of the policy version.
-- * 'isDefaultVersion' - Specifies whether the policy version is the default.
-- * 'responseStatus' - The response status code.
mkGetPolicyVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPolicyVersionResponse
mkGetPolicyVersionResponse pResponseStatus_ =
  GetPolicyVersionResponse'
    { lastModifiedDate = Lude.Nothing,
      policyName = Lude.Nothing,
      policyDocument = Lude.Nothing,
      policyVersionId = Lude.Nothing,
      policyARN = Lude.Nothing,
      creationDate = Lude.Nothing,
      generationId = Lude.Nothing,
      isDefaultVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date the policy was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsLastModifiedDate :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe Lude.Timestamp)
gpvrsLastModifiedDate = Lens.lens (lastModifiedDate :: GetPolicyVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsPolicyName :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe Lude.Text)
gpvrsPolicyName = Lens.lens (policyName :: GetPolicyVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON document that describes the policy.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsPolicyDocument :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe Lude.Text)
gpvrsPolicyDocument = Lens.lens (policyDocument :: GetPolicyVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsPolicyVersionId :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe Lude.Text)
gpvrsPolicyVersionId = Lens.lens (policyVersionId :: GetPolicyVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyVersionId = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsPolicyARN :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe Lude.Text)
gpvrsPolicyARN = Lens.lens (policyARN :: GetPolicyVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The date the policy was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsCreationDate :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe Lude.Timestamp)
gpvrsCreationDate = Lens.lens (creationDate :: GetPolicyVersionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The generation ID of the policy version.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsGenerationId :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe Lude.Text)
gpvrsGenerationId = Lens.lens (generationId :: GetPolicyVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {generationId = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsGenerationId "Use generic-lens or generic-optics with 'generationId' instead." #-}

-- | Specifies whether the policy version is the default.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsIsDefaultVersion :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe Lude.Bool)
gpvrsIsDefaultVersion = Lens.lens (isDefaultVersion :: GetPolicyVersionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsResponseStatus :: Lens.Lens' GetPolicyVersionResponse Lude.Int
gpvrsResponseStatus = Lens.lens (responseStatus :: GetPolicyVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

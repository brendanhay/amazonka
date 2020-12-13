{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetLayerVersionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the permission policy for a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . For more information, see 'AddLayerVersionPermission' .
module Network.AWS.Lambda.GetLayerVersionPolicy
  ( -- * Creating a request
    GetLayerVersionPolicy (..),
    mkGetLayerVersionPolicy,

    -- ** Request lenses
    glvpLayerName,
    glvpVersionNumber,

    -- * Destructuring the response
    GetLayerVersionPolicyResponse (..),
    mkGetLayerVersionPolicyResponse,

    -- ** Response lenses
    glvprsPolicy,
    glvprsRevisionId,
    glvprsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLayerVersionPolicy' smart constructor.
data GetLayerVersionPolicy = GetLayerVersionPolicy'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Lude.Text,
    -- | The version number.
    versionNumber :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLayerVersionPolicy' with the minimum fields required to make a request.
--
-- * 'layerName' - The name or Amazon Resource Name (ARN) of the layer.
-- * 'versionNumber' - The version number.
mkGetLayerVersionPolicy ::
  -- | 'layerName'
  Lude.Text ->
  -- | 'versionNumber'
  Lude.Integer ->
  GetLayerVersionPolicy
mkGetLayerVersionPolicy pLayerName_ pVersionNumber_ =
  GetLayerVersionPolicy'
    { layerName = pLayerName_,
      versionNumber = pVersionNumber_
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvpLayerName :: Lens.Lens' GetLayerVersionPolicy Lude.Text
glvpLayerName = Lens.lens (layerName :: GetLayerVersionPolicy -> Lude.Text) (\s a -> s {layerName = a} :: GetLayerVersionPolicy)
{-# DEPRECATED glvpLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvpVersionNumber :: Lens.Lens' GetLayerVersionPolicy Lude.Integer
glvpVersionNumber = Lens.lens (versionNumber :: GetLayerVersionPolicy -> Lude.Integer) (\s a -> s {versionNumber = a} :: GetLayerVersionPolicy)
{-# DEPRECATED glvpVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Lude.AWSRequest GetLayerVersionPolicy where
  type Rs GetLayerVersionPolicy = GetLayerVersionPolicyResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLayerVersionPolicyResponse'
            Lude.<$> (x Lude..?> "Policy")
            Lude.<*> (x Lude..?> "RevisionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLayerVersionPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetLayerVersionPolicy where
  toPath GetLayerVersionPolicy' {..} =
    Lude.mconcat
      [ "/2018-10-31/layers/",
        Lude.toBS layerName,
        "/versions/",
        Lude.toBS versionNumber,
        "/policy"
      ]

instance Lude.ToQuery GetLayerVersionPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLayerVersionPolicyResponse' smart constructor.
data GetLayerVersionPolicyResponse = GetLayerVersionPolicyResponse'
  { -- | The policy document.
    policy :: Lude.Maybe Lude.Text,
    -- | A unique identifier for the current revision of the policy.
    revisionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLayerVersionPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - The policy document.
-- * 'revisionId' - A unique identifier for the current revision of the policy.
-- * 'responseStatus' - The response status code.
mkGetLayerVersionPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLayerVersionPolicyResponse
mkGetLayerVersionPolicyResponse pResponseStatus_ =
  GetLayerVersionPolicyResponse'
    { policy = Lude.Nothing,
      revisionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The policy document.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvprsPolicy :: Lens.Lens' GetLayerVersionPolicyResponse (Lude.Maybe Lude.Text)
glvprsPolicy = Lens.lens (policy :: GetLayerVersionPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: GetLayerVersionPolicyResponse)
{-# DEPRECATED glvprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | A unique identifier for the current revision of the policy.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvprsRevisionId :: Lens.Lens' GetLayerVersionPolicyResponse (Lude.Maybe Lude.Text)
glvprsRevisionId = Lens.lens (revisionId :: GetLayerVersionPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: GetLayerVersionPolicyResponse)
{-# DEPRECATED glvprsRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvprsResponseStatus :: Lens.Lens' GetLayerVersionPolicyResponse Lude.Int
glvprsResponseStatus = Lens.lens (responseStatus :: GetLayerVersionPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLayerVersionPolicyResponse)
{-# DEPRECATED glvprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

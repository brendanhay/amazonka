{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.UpdateBasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the 'BasePathMapping' resource.
module Network.AWS.APIGateway.UpdateBasePathMapping
  ( -- * Creating a request
    UpdateBasePathMapping (..),
    mkUpdateBasePathMapping,

    -- ** Request lenses
    ubpmPatchOperations,
    ubpmDomainName,
    ubpmBasePath,

    -- * Destructuring the response
    BasePathMapping (..),
    mkBasePathMapping,

    -- ** Response lenses
    bpmStage,
    bpmBasePath,
    bpmRestAPIId,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to change information about the 'BasePathMapping' resource.
--
-- /See:/ 'mkUpdateBasePathMapping' smart constructor.
data UpdateBasePathMapping = UpdateBasePathMapping'
  { patchOperations ::
      Lude.Maybe [PatchOperation],
    domainName :: Lude.Text,
    basePath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBasePathMapping' with the minimum fields required to make a request.
--
-- * 'basePath' - [Required] The base path of the 'BasePathMapping' resource to change.
--
-- To specify an empty base path, set this parameter to @'(none)'@ .
-- * 'domainName' - [Required] The domain name of the 'BasePathMapping' resource to change.
-- * 'patchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
mkUpdateBasePathMapping ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'basePath'
  Lude.Text ->
  UpdateBasePathMapping
mkUpdateBasePathMapping pDomainName_ pBasePath_ =
  UpdateBasePathMapping'
    { patchOperations = Lude.Nothing,
      domainName = pDomainName_,
      basePath = pBasePath_
    }

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpmPatchOperations :: Lens.Lens' UpdateBasePathMapping (Lude.Maybe [PatchOperation])
ubpmPatchOperations = Lens.lens (patchOperations :: UpdateBasePathMapping -> Lude.Maybe [PatchOperation]) (\s a -> s {patchOperations = a} :: UpdateBasePathMapping)
{-# DEPRECATED ubpmPatchOperations "Use generic-lens or generic-optics with 'patchOperations' instead." #-}

-- | [Required] The domain name of the 'BasePathMapping' resource to change.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpmDomainName :: Lens.Lens' UpdateBasePathMapping Lude.Text
ubpmDomainName = Lens.lens (domainName :: UpdateBasePathMapping -> Lude.Text) (\s a -> s {domainName = a} :: UpdateBasePathMapping)
{-# DEPRECATED ubpmDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | [Required] The base path of the 'BasePathMapping' resource to change.
--
-- To specify an empty base path, set this parameter to @'(none)'@ .
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpmBasePath :: Lens.Lens' UpdateBasePathMapping Lude.Text
ubpmBasePath = Lens.lens (basePath :: UpdateBasePathMapping -> Lude.Text) (\s a -> s {basePath = a} :: UpdateBasePathMapping)
{-# DEPRECATED ubpmBasePath "Use generic-lens or generic-optics with 'basePath' instead." #-}

instance Lude.AWSRequest UpdateBasePathMapping where
  type Rs UpdateBasePathMapping = BasePathMapping
  request = Req.patchJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateBasePathMapping where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON UpdateBasePathMapping where
  toJSON UpdateBasePathMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [("patchOperations" Lude..=) Lude.<$> patchOperations]
      )

instance Lude.ToPath UpdateBasePathMapping where
  toPath UpdateBasePathMapping' {..} =
    Lude.mconcat
      [ "/domainnames/",
        Lude.toBS domainName,
        "/basepathmappings/",
        Lude.toBS basePath
      ]

instance Lude.ToQuery UpdateBasePathMapping where
  toQuery = Lude.const Lude.mempty

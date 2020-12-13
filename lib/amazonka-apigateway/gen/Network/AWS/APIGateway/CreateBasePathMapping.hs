{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateBasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'BasePathMapping' resource.
module Network.AWS.APIGateway.CreateBasePathMapping
  ( -- * Creating a request
    CreateBasePathMapping (..),
    mkCreateBasePathMapping,

    -- ** Request lenses
    cbpmStage,
    cbpmBasePath,
    cbpmDomainName,
    cbpmRestAPIId,

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

-- | Requests API Gateway to create a new 'BasePathMapping' resource.
--
-- /See:/ 'mkCreateBasePathMapping' smart constructor.
data CreateBasePathMapping = CreateBasePathMapping'
  { -- | The name of the API's stage that you want to use for this mapping. Specify '(none)' if you want callers to explicitly specify the stage name after any base path name.
    stage :: Lude.Maybe Lude.Text,
    -- | The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Specify '(none)' if you do not want callers to specify a base path name after the domain name.
    basePath :: Lude.Maybe Lude.Text,
    -- | [Required] The domain name of the 'BasePathMapping' resource to create.
    domainName :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBasePathMapping' with the minimum fields required to make a request.
--
-- * 'stage' - The name of the API's stage that you want to use for this mapping. Specify '(none)' if you want callers to explicitly specify the stage name after any base path name.
-- * 'basePath' - The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Specify '(none)' if you do not want callers to specify a base path name after the domain name.
-- * 'domainName' - [Required] The domain name of the 'BasePathMapping' resource to create.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkCreateBasePathMapping ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  CreateBasePathMapping
mkCreateBasePathMapping pDomainName_ pRestAPIId_ =
  CreateBasePathMapping'
    { stage = Lude.Nothing,
      basePath = Lude.Nothing,
      domainName = pDomainName_,
      restAPIId = pRestAPIId_
    }

-- | The name of the API's stage that you want to use for this mapping. Specify '(none)' if you want callers to explicitly specify the stage name after any base path name.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpmStage :: Lens.Lens' CreateBasePathMapping (Lude.Maybe Lude.Text)
cbpmStage = Lens.lens (stage :: CreateBasePathMapping -> Lude.Maybe Lude.Text) (\s a -> s {stage = a} :: CreateBasePathMapping)
{-# DEPRECATED cbpmStage "Use generic-lens or generic-optics with 'stage' instead." #-}

-- | The base path name that callers of the API must provide as part of the URL after the domain name. This value must be unique for all of the mappings across a single API. Specify '(none)' if you do not want callers to specify a base path name after the domain name.
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpmBasePath :: Lens.Lens' CreateBasePathMapping (Lude.Maybe Lude.Text)
cbpmBasePath = Lens.lens (basePath :: CreateBasePathMapping -> Lude.Maybe Lude.Text) (\s a -> s {basePath = a} :: CreateBasePathMapping)
{-# DEPRECATED cbpmBasePath "Use generic-lens or generic-optics with 'basePath' instead." #-}

-- | [Required] The domain name of the 'BasePathMapping' resource to create.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpmDomainName :: Lens.Lens' CreateBasePathMapping Lude.Text
cbpmDomainName = Lens.lens (domainName :: CreateBasePathMapping -> Lude.Text) (\s a -> s {domainName = a} :: CreateBasePathMapping)
{-# DEPRECATED cbpmDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpmRestAPIId :: Lens.Lens' CreateBasePathMapping Lude.Text
cbpmRestAPIId = Lens.lens (restAPIId :: CreateBasePathMapping -> Lude.Text) (\s a -> s {restAPIId = a} :: CreateBasePathMapping)
{-# DEPRECATED cbpmRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest CreateBasePathMapping where
  type Rs CreateBasePathMapping = BasePathMapping
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateBasePathMapping where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateBasePathMapping where
  toJSON CreateBasePathMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stage" Lude..=) Lude.<$> stage,
            ("basePath" Lude..=) Lude.<$> basePath,
            Lude.Just ("restApiId" Lude..= restAPIId)
          ]
      )

instance Lude.ToPath CreateBasePathMapping where
  toPath CreateBasePathMapping' {..} =
    Lude.mconcat
      ["/domainnames/", Lude.toBS domainName, "/basepathmappings"]

instance Lude.ToQuery CreateBasePathMapping where
  toQuery = Lude.const Lude.mempty

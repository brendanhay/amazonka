{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Model' resource to an existing 'RestApi' resource.
module Network.AWS.APIGateway.CreateModel
  ( -- * Creating a request
    CreateModel (..),
    mkCreateModel,

    -- ** Request lenses
    cmSchema,
    cmName,
    cmRestAPIId,
    cmDescription,
    cmContentType,

    -- * Destructuring the response
    Model (..),
    mkModel,

    -- ** Response lenses
    mSchema,
    mName,
    mId,
    mDescription,
    mContentType,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to add a new 'Model' to an existing 'RestApi' resource.
--
-- /See:/ 'mkCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | The schema for the model. For @application/json@ models, this should be <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4> model.
    schema :: Lude.Maybe Lude.Text,
    -- | [Required] The name of the model. Must be alphanumeric.
    name :: Lude.Text,
    -- | [Required] The 'RestApi' identifier under which the 'Model' will be created.
    restAPIId :: Lude.Text,
    -- | The description of the model.
    description :: Lude.Maybe Lude.Text,
    -- | [Required] The content-type for the model.
    contentType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateModel' with the minimum fields required to make a request.
--
-- * 'schema' - The schema for the model. For @application/json@ models, this should be <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4> model.
-- * 'name' - [Required] The name of the model. Must be alphanumeric.
-- * 'restAPIId' - [Required] The 'RestApi' identifier under which the 'Model' will be created.
-- * 'description' - The description of the model.
-- * 'contentType' - [Required] The content-type for the model.
mkCreateModel ::
  -- | 'name'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'contentType'
  Lude.Text ->
  CreateModel
mkCreateModel pName_ pRestAPIId_ pContentType_ =
  CreateModel'
    { schema = Lude.Nothing,
      name = pName_,
      restAPIId = pRestAPIId_,
      description = Lude.Nothing,
      contentType = pContentType_
    }

-- | The schema for the model. For @application/json@ models, this should be <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4> model.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmSchema :: Lens.Lens' CreateModel (Lude.Maybe Lude.Text)
cmSchema = Lens.lens (schema :: CreateModel -> Lude.Maybe Lude.Text) (\s a -> s {schema = a} :: CreateModel)
{-# DEPRECATED cmSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | [Required] The name of the model. Must be alphanumeric.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' CreateModel Lude.Text
cmName = Lens.lens (name :: CreateModel -> Lude.Text) (\s a -> s {name = a} :: CreateModel)
{-# DEPRECATED cmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | [Required] The 'RestApi' identifier under which the 'Model' will be created.
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRestAPIId :: Lens.Lens' CreateModel Lude.Text
cmRestAPIId = Lens.lens (restAPIId :: CreateModel -> Lude.Text) (\s a -> s {restAPIId = a} :: CreateModel)
{-# DEPRECATED cmRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The description of the model.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmDescription :: Lens.Lens' CreateModel (Lude.Maybe Lude.Text)
cmDescription = Lens.lens (description :: CreateModel -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateModel)
{-# DEPRECATED cmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | [Required] The content-type for the model.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContentType :: Lens.Lens' CreateModel Lude.Text
cmContentType = Lens.lens (contentType :: CreateModel -> Lude.Text) (\s a -> s {contentType = a} :: CreateModel)
{-# DEPRECATED cmContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.AWSRequest CreateModel where
  type Rs CreateModel = Model
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("schema" Lude..=) Lude.<$> schema,
            Lude.Just ("name" Lude..= name),
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("contentType" Lude..= contentType)
          ]
      )

instance Lude.ToPath CreateModel where
  toPath CreateModel' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId, "/models"]

instance Lude.ToQuery CreateModel where
  toQuery = Lude.const Lude.mempty

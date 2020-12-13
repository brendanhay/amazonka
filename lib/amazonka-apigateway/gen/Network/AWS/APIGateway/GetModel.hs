{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing model defined for a 'RestApi' resource.
module Network.AWS.APIGateway.GetModel
  ( -- * Creating a request
    GetModel (..),
    mkGetModel,

    -- ** Request lenses
    gmModelName,
    gmRestAPIId,
    gmFlatten,

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

-- | Request to list information about a model in an existing 'RestApi' resource.
--
-- /See:/ 'mkGetModel' smart constructor.
data GetModel = GetModel'
  { -- | [Required] The name of the model as an identifier.
    modelName :: Lude.Text,
    -- | [Required] The 'RestApi' identifier under which the 'Model' exists.
    restAPIId :: Lude.Text,
    -- | A query parameter of a Boolean value to resolve (@true@ ) all external model references and returns a flattened model schema or not (@false@ ) The default is @false@ .
    flatten :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetModel' with the minimum fields required to make a request.
--
-- * 'modelName' - [Required] The name of the model as an identifier.
-- * 'restAPIId' - [Required] The 'RestApi' identifier under which the 'Model' exists.
-- * 'flatten' - A query parameter of a Boolean value to resolve (@true@ ) all external model references and returns a flattened model schema or not (@false@ ) The default is @false@ .
mkGetModel ::
  -- | 'modelName'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  GetModel
mkGetModel pModelName_ pRestAPIId_ =
  GetModel'
    { modelName = pModelName_,
      restAPIId = pRestAPIId_,
      flatten = Lude.Nothing
    }

-- | [Required] The name of the model as an identifier.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmModelName :: Lens.Lens' GetModel Lude.Text
gmModelName = Lens.lens (modelName :: GetModel -> Lude.Text) (\s a -> s {modelName = a} :: GetModel)
{-# DEPRECATED gmModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | [Required] The 'RestApi' identifier under which the 'Model' exists.
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmRestAPIId :: Lens.Lens' GetModel Lude.Text
gmRestAPIId = Lens.lens (restAPIId :: GetModel -> Lude.Text) (\s a -> s {restAPIId = a} :: GetModel)
{-# DEPRECATED gmRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | A query parameter of a Boolean value to resolve (@true@ ) all external model references and returns a flattened model schema or not (@false@ ) The default is @false@ .
--
-- /Note:/ Consider using 'flatten' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmFlatten :: Lens.Lens' GetModel (Lude.Maybe Lude.Bool)
gmFlatten = Lens.lens (flatten :: GetModel -> Lude.Maybe Lude.Bool) (\s a -> s {flatten = a} :: GetModel)
{-# DEPRECATED gmFlatten "Use generic-lens or generic-optics with 'flatten' instead." #-}

instance Lude.AWSRequest GetModel where
  type Rs GetModel = Model
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetModel where
  toPath GetModel' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/models/",
        Lude.toBS modelName
      ]

instance Lude.ToQuery GetModel where
  toQuery GetModel' {..} = Lude.mconcat ["flatten" Lude.=: flatten]

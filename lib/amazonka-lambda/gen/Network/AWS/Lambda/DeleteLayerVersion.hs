{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteLayerVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . Deleted versions can no longer be viewed or added to functions. To avoid breaking functions, a copy of the version remains in Lambda until no functions refer to it.
module Network.AWS.Lambda.DeleteLayerVersion
  ( -- * Creating a request
    DeleteLayerVersion (..),
    mkDeleteLayerVersion,

    -- ** Request lenses
    dlvLayerName,
    dlvVersionNumber,

    -- * Destructuring the response
    DeleteLayerVersionResponse (..),
    mkDeleteLayerVersionResponse,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLayerVersion' smart constructor.
data DeleteLayerVersion = DeleteLayerVersion'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Lude.Text,
    -- | The version number.
    versionNumber :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLayerVersion' with the minimum fields required to make a request.
--
-- * 'layerName' - The name or Amazon Resource Name (ARN) of the layer.
-- * 'versionNumber' - The version number.
mkDeleteLayerVersion ::
  -- | 'layerName'
  Lude.Text ->
  -- | 'versionNumber'
  Lude.Integer ->
  DeleteLayerVersion
mkDeleteLayerVersion pLayerName_ pVersionNumber_ =
  DeleteLayerVersion'
    { layerName = pLayerName_,
      versionNumber = pVersionNumber_
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlvLayerName :: Lens.Lens' DeleteLayerVersion Lude.Text
dlvLayerName = Lens.lens (layerName :: DeleteLayerVersion -> Lude.Text) (\s a -> s {layerName = a} :: DeleteLayerVersion)
{-# DEPRECATED dlvLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlvVersionNumber :: Lens.Lens' DeleteLayerVersion Lude.Integer
dlvVersionNumber = Lens.lens (versionNumber :: DeleteLayerVersion -> Lude.Integer) (\s a -> s {versionNumber = a} :: DeleteLayerVersion)
{-# DEPRECATED dlvVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Lude.AWSRequest DeleteLayerVersion where
  type Rs DeleteLayerVersion = DeleteLayerVersionResponse
  request = Req.delete lambdaService
  response = Res.receiveNull DeleteLayerVersionResponse'

instance Lude.ToHeaders DeleteLayerVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLayerVersion where
  toPath DeleteLayerVersion' {..} =
    Lude.mconcat
      [ "/2018-10-31/layers/",
        Lude.toBS layerName,
        "/versions/",
        Lude.toBS versionNumber
      ]

instance Lude.ToQuery DeleteLayerVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLayerVersionResponse' smart constructor.
data DeleteLayerVersionResponse = DeleteLayerVersionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLayerVersionResponse' with the minimum fields required to make a request.
mkDeleteLayerVersionResponse ::
  DeleteLayerVersionResponse
mkDeleteLayerVersionResponse = DeleteLayerVersionResponse'

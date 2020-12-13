{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'Stage' resource.
module Network.AWS.APIGateway.DeleteStage
  ( -- * Creating a request
    DeleteStage (..),
    mkDeleteStage,

    -- ** Request lenses
    dsRestAPIId,
    dsStageName,

    -- * Destructuring the response
    DeleteStageResponse (..),
    mkDeleteStageResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Requests API Gateway to delete a 'Stage' resource.
--
-- /See:/ 'mkDeleteStage' smart constructor.
data DeleteStage = DeleteStage'
  { -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | [Required] The name of the 'Stage' resource to delete.
    stageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStage' with the minimum fields required to make a request.
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'stageName' - [Required] The name of the 'Stage' resource to delete.
mkDeleteStage ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'stageName'
  Lude.Text ->
  DeleteStage
mkDeleteStage pRestAPIId_ pStageName_ =
  DeleteStage' {restAPIId = pRestAPIId_, stageName = pStageName_}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRestAPIId :: Lens.Lens' DeleteStage Lude.Text
dsRestAPIId = Lens.lens (restAPIId :: DeleteStage -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteStage)
{-# DEPRECATED dsRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The name of the 'Stage' resource to delete.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStageName :: Lens.Lens' DeleteStage Lude.Text
dsStageName = Lens.lens (stageName :: DeleteStage -> Lude.Text) (\s a -> s {stageName = a} :: DeleteStage)
{-# DEPRECATED dsStageName "Use generic-lens or generic-optics with 'stageName' instead." #-}

instance Lude.AWSRequest DeleteStage where
  type Rs DeleteStage = DeleteStageResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteStageResponse'

instance Lude.ToHeaders DeleteStage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteStage where
  toPath DeleteStage' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/stages/",
        Lude.toBS stageName
      ]

instance Lude.ToQuery DeleteStage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStageResponse' smart constructor.
data DeleteStageResponse = DeleteStageResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStageResponse' with the minimum fields required to make a request.
mkDeleteStageResponse ::
  DeleteStageResponse
mkDeleteStageResponse = DeleteStageResponse'

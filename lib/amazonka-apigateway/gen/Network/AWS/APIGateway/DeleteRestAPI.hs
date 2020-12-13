{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteRestAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified API.
module Network.AWS.APIGateway.DeleteRestAPI
  ( -- * Creating a request
    DeleteRestAPI (..),
    mkDeleteRestAPI,

    -- ** Request lenses
    draRestAPIId,

    -- * Destructuring the response
    DeleteRestAPIResponse (..),
    mkDeleteRestAPIResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to delete the specified API from your collection.
--
-- /See:/ 'mkDeleteRestAPI' smart constructor.
newtype DeleteRestAPI = DeleteRestAPI'
  { -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRestAPI' with the minimum fields required to make a request.
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteRestAPI ::
  -- | 'restAPIId'
  Lude.Text ->
  DeleteRestAPI
mkDeleteRestAPI pRestAPIId_ =
  DeleteRestAPI' {restAPIId = pRestAPIId_}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draRestAPIId :: Lens.Lens' DeleteRestAPI Lude.Text
draRestAPIId = Lens.lens (restAPIId :: DeleteRestAPI -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteRestAPI)
{-# DEPRECATED draRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest DeleteRestAPI where
  type Rs DeleteRestAPI = DeleteRestAPIResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteRestAPIResponse'

instance Lude.ToHeaders DeleteRestAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteRestAPI where
  toPath DeleteRestAPI' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId]

instance Lude.ToQuery DeleteRestAPI where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRestAPIResponse' smart constructor.
data DeleteRestAPIResponse = DeleteRestAPIResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRestAPIResponse' with the minimum fields required to make a request.
mkDeleteRestAPIResponse ::
  DeleteRestAPIResponse
mkDeleteRestAPIResponse = DeleteRestAPIResponse'

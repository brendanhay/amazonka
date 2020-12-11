{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteRequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'RequestValidator' of a given 'RestApi' .
module Network.AWS.APIGateway.DeleteRequestValidator
  ( -- * Creating a request
    DeleteRequestValidator (..),
    mkDeleteRequestValidator,

    -- ** Request lenses
    drvRestAPIId,
    drvRequestValidatorId,

    -- * Destructuring the response
    DeleteRequestValidatorResponse (..),
    mkDeleteRequestValidatorResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes a specified 'RequestValidator' of a given 'RestApi' .
--
-- /See:/ 'mkDeleteRequestValidator' smart constructor.
data DeleteRequestValidator = DeleteRequestValidator'
  { restAPIId ::
      Lude.Text,
    requestValidatorId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRequestValidator' with the minimum fields required to make a request.
--
-- * 'requestValidatorId' - [Required] The identifier of the 'RequestValidator' to be deleted.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkDeleteRequestValidator ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'requestValidatorId'
  Lude.Text ->
  DeleteRequestValidator
mkDeleteRequestValidator pRestAPIId_ pRequestValidatorId_ =
  DeleteRequestValidator'
    { restAPIId = pRestAPIId_,
      requestValidatorId = pRequestValidatorId_
    }

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drvRestAPIId :: Lens.Lens' DeleteRequestValidator Lude.Text
drvRestAPIId = Lens.lens (restAPIId :: DeleteRequestValidator -> Lude.Text) (\s a -> s {restAPIId = a} :: DeleteRequestValidator)
{-# DEPRECATED drvRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The identifier of the 'RequestValidator' to be deleted.
--
-- /Note:/ Consider using 'requestValidatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drvRequestValidatorId :: Lens.Lens' DeleteRequestValidator Lude.Text
drvRequestValidatorId = Lens.lens (requestValidatorId :: DeleteRequestValidator -> Lude.Text) (\s a -> s {requestValidatorId = a} :: DeleteRequestValidator)
{-# DEPRECATED drvRequestValidatorId "Use generic-lens or generic-optics with 'requestValidatorId' instead." #-}

instance Lude.AWSRequest DeleteRequestValidator where
  type Rs DeleteRequestValidator = DeleteRequestValidatorResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteRequestValidatorResponse'

instance Lude.ToHeaders DeleteRequestValidator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteRequestValidator where
  toPath DeleteRequestValidator' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/requestvalidators/",
        Lude.toBS requestValidatorId
      ]

instance Lude.ToQuery DeleteRequestValidator where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRequestValidatorResponse' smart constructor.
data DeleteRequestValidatorResponse = DeleteRequestValidatorResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRequestValidatorResponse' with the minimum fields required to make a request.
mkDeleteRequestValidatorResponse ::
  DeleteRequestValidatorResponse
mkDeleteRequestValidatorResponse = DeleteRequestValidatorResponse'

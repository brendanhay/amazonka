{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a parameter from the system.
module Network.AWS.SSM.DeleteParameter
  ( -- * Creating a request
    DeleteParameter (..),
    mkDeleteParameter,

    -- ** Request lenses
    dName,

    -- * Destructuring the response
    DeleteParameterResponse (..),
    mkDeleteParameterResponse,

    -- ** Response lenses
    dpfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeleteParameter' smart constructor.
newtype DeleteParameter = DeleteParameter'
  { -- | The name of the parameter to delete.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteParameter' with the minimum fields required to make a request.
--
-- * 'name' - The name of the parameter to delete.
mkDeleteParameter ::
  -- | 'name'
  Lude.Text ->
  DeleteParameter
mkDeleteParameter pName_ = DeleteParameter' {name = pName_}

-- | The name of the parameter to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DeleteParameter Lude.Text
dName = Lens.lens (name :: DeleteParameter -> Lude.Text) (\s a -> s {name = a} :: DeleteParameter)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteParameter where
  type Rs DeleteParameter = DeleteParameterResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteParameterResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteParameter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeleteParameter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteParameter where
  toJSON DeleteParameter' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteParameter where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteParameter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteParameterResponse' smart constructor.
newtype DeleteParameterResponse = DeleteParameterResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteParameterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteParameterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteParameterResponse
mkDeleteParameterResponse pResponseStatus_ =
  DeleteParameterResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfrsResponseStatus :: Lens.Lens' DeleteParameterResponse Lude.Int
dpfrsResponseStatus = Lens.lens (responseStatus :: DeleteParameterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteParameterResponse)
{-# DEPRECATED dpfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

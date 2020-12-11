{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a workforce.
--
-- If you want to create a new workforce in an AWS Region where a workforce already exists, use this operation to delete the existing workforce and then use to create a new workforce.
-- /Important:/ If a private workforce contains one or more work teams, you must use the operation to delete all work teams before you delete the workforce. If you try to delete a workforce that contains one or more work teams, you will recieve a @ResourceInUse@ error.
module Network.AWS.SageMaker.DeleteWorkforce
  ( -- * Creating a request
    DeleteWorkforce (..),
    mkDeleteWorkforce,

    -- ** Request lenses
    dwWorkforceName,

    -- * Destructuring the response
    DeleteWorkforceResponse (..),
    mkDeleteWorkforceResponse,

    -- ** Response lenses
    deleteworkforceersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteWorkforce' smart constructor.
newtype DeleteWorkforce = DeleteWorkforce'
  { workforceName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkforce' with the minimum fields required to make a request.
--
-- * 'workforceName' - The name of the workforce.
mkDeleteWorkforce ::
  -- | 'workforceName'
  Lude.Text ->
  DeleteWorkforce
mkDeleteWorkforce pWorkforceName_ =
  DeleteWorkforce' {workforceName = pWorkforceName_}

-- | The name of the workforce.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwWorkforceName :: Lens.Lens' DeleteWorkforce Lude.Text
dwWorkforceName = Lens.lens (workforceName :: DeleteWorkforce -> Lude.Text) (\s a -> s {workforceName = a} :: DeleteWorkforce)
{-# DEPRECATED dwWorkforceName "Use generic-lens or generic-optics with 'workforceName' instead." #-}

instance Lude.AWSRequest DeleteWorkforce where
  type Rs DeleteWorkforce = DeleteWorkforceResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteWorkforceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteWorkforce where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteWorkforce" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWorkforce where
  toJSON DeleteWorkforce' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WorkforceName" Lude..= workforceName)]
      )

instance Lude.ToPath DeleteWorkforce where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteWorkforce where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteWorkforceResponse' smart constructor.
newtype DeleteWorkforceResponse = DeleteWorkforceResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkforceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteWorkforceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteWorkforceResponse
mkDeleteWorkforceResponse pResponseStatus_ =
  DeleteWorkforceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deleteworkforceersResponseStatus :: Lens.Lens' DeleteWorkforceResponse Lude.Int
deleteworkforceersResponseStatus = Lens.lens (responseStatus :: DeleteWorkforceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteWorkforceResponse)
{-# DEPRECATED deleteworkforceersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.BatchDisassociateUserStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified users from the specified stacks.
module Network.AWS.AppStream.BatchDisassociateUserStack
  ( -- * Creating a request
    BatchDisassociateUserStack (..),
    mkBatchDisassociateUserStack,

    -- ** Request lenses
    bdusUserStackAssociations,

    -- * Destructuring the response
    BatchDisassociateUserStackResponse (..),
    mkBatchDisassociateUserStackResponse,

    -- ** Response lenses
    bdusrsErrors,
    bdusrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDisassociateUserStack' smart constructor.
newtype BatchDisassociateUserStack = BatchDisassociateUserStack'
  { -- | The list of UserStackAssociation objects.
    userStackAssociations :: Lude.NonEmpty UserStackAssociation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDisassociateUserStack' with the minimum fields required to make a request.
--
-- * 'userStackAssociations' - The list of UserStackAssociation objects.
mkBatchDisassociateUserStack ::
  -- | 'userStackAssociations'
  Lude.NonEmpty UserStackAssociation ->
  BatchDisassociateUserStack
mkBatchDisassociateUserStack pUserStackAssociations_ =
  BatchDisassociateUserStack'
    { userStackAssociations =
        pUserStackAssociations_
    }

-- | The list of UserStackAssociation objects.
--
-- /Note:/ Consider using 'userStackAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdusUserStackAssociations :: Lens.Lens' BatchDisassociateUserStack (Lude.NonEmpty UserStackAssociation)
bdusUserStackAssociations = Lens.lens (userStackAssociations :: BatchDisassociateUserStack -> Lude.NonEmpty UserStackAssociation) (\s a -> s {userStackAssociations = a} :: BatchDisassociateUserStack)
{-# DEPRECATED bdusUserStackAssociations "Use generic-lens or generic-optics with 'userStackAssociations' instead." #-}

instance Lude.AWSRequest BatchDisassociateUserStack where
  type
    Rs BatchDisassociateUserStack =
      BatchDisassociateUserStackResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchDisassociateUserStackResponse'
            Lude.<$> (x Lude..?> "errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDisassociateUserStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.BatchDisassociateUserStack" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchDisassociateUserStack where
  toJSON BatchDisassociateUserStack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("UserStackAssociations" Lude..= userStackAssociations)
          ]
      )

instance Lude.ToPath BatchDisassociateUserStack where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDisassociateUserStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchDisassociateUserStackResponse' smart constructor.
data BatchDisassociateUserStackResponse = BatchDisassociateUserStackResponse'
  { -- | The list of UserStackAssociationError objects.
    errors :: Lude.Maybe [UserStackAssociationError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDisassociateUserStackResponse' with the minimum fields required to make a request.
--
-- * 'errors' - The list of UserStackAssociationError objects.
-- * 'responseStatus' - The response status code.
mkBatchDisassociateUserStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDisassociateUserStackResponse
mkBatchDisassociateUserStackResponse pResponseStatus_ =
  BatchDisassociateUserStackResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of UserStackAssociationError objects.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdusrsErrors :: Lens.Lens' BatchDisassociateUserStackResponse (Lude.Maybe [UserStackAssociationError])
bdusrsErrors = Lens.lens (errors :: BatchDisassociateUserStackResponse -> Lude.Maybe [UserStackAssociationError]) (\s a -> s {errors = a} :: BatchDisassociateUserStackResponse)
{-# DEPRECATED bdusrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdusrsResponseStatus :: Lens.Lens' BatchDisassociateUserStackResponse Lude.Int
bdusrsResponseStatus = Lens.lens (responseStatus :: BatchDisassociateUserStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDisassociateUserStackResponse)
{-# DEPRECATED bdusrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

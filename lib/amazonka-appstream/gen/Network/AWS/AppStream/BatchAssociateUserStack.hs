{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.BatchAssociateUserStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified users with the specified stacks. Users in a user pool cannot be assigned to stacks with fleets that are joined to an Active Directory domain.
module Network.AWS.AppStream.BatchAssociateUserStack
  ( -- * Creating a request
    BatchAssociateUserStack (..),
    mkBatchAssociateUserStack,

    -- ** Request lenses
    bausUserStackAssociations,

    -- * Destructuring the response
    BatchAssociateUserStackResponse (..),
    mkBatchAssociateUserStackResponse,

    -- ** Response lenses
    bausrsErrors,
    bausrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchAssociateUserStack' smart constructor.
newtype BatchAssociateUserStack = BatchAssociateUserStack'
  { -- | The list of UserStackAssociation objects.
    userStackAssociations :: Lude.NonEmpty UserStackAssociation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAssociateUserStack' with the minimum fields required to make a request.
--
-- * 'userStackAssociations' - The list of UserStackAssociation objects.
mkBatchAssociateUserStack ::
  -- | 'userStackAssociations'
  Lude.NonEmpty UserStackAssociation ->
  BatchAssociateUserStack
mkBatchAssociateUserStack pUserStackAssociations_ =
  BatchAssociateUserStack'
    { userStackAssociations =
        pUserStackAssociations_
    }

-- | The list of UserStackAssociation objects.
--
-- /Note:/ Consider using 'userStackAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bausUserStackAssociations :: Lens.Lens' BatchAssociateUserStack (Lude.NonEmpty UserStackAssociation)
bausUserStackAssociations = Lens.lens (userStackAssociations :: BatchAssociateUserStack -> Lude.NonEmpty UserStackAssociation) (\s a -> s {userStackAssociations = a} :: BatchAssociateUserStack)
{-# DEPRECATED bausUserStackAssociations "Use generic-lens or generic-optics with 'userStackAssociations' instead." #-}

instance Lude.AWSRequest BatchAssociateUserStack where
  type Rs BatchAssociateUserStack = BatchAssociateUserStackResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchAssociateUserStackResponse'
            Lude.<$> (x Lude..?> "errors" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchAssociateUserStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.BatchAssociateUserStack" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchAssociateUserStack where
  toJSON BatchAssociateUserStack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("UserStackAssociations" Lude..= userStackAssociations)
          ]
      )

instance Lude.ToPath BatchAssociateUserStack where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchAssociateUserStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchAssociateUserStackResponse' smart constructor.
data BatchAssociateUserStackResponse = BatchAssociateUserStackResponse'
  { -- | The list of UserStackAssociationError objects.
    errors :: Lude.Maybe [UserStackAssociationError],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAssociateUserStackResponse' with the minimum fields required to make a request.
--
-- * 'errors' - The list of UserStackAssociationError objects.
-- * 'responseStatus' - The response status code.
mkBatchAssociateUserStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchAssociateUserStackResponse
mkBatchAssociateUserStackResponse pResponseStatus_ =
  BatchAssociateUserStackResponse'
    { errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of UserStackAssociationError objects.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bausrsErrors :: Lens.Lens' BatchAssociateUserStackResponse (Lude.Maybe [UserStackAssociationError])
bausrsErrors = Lens.lens (errors :: BatchAssociateUserStackResponse -> Lude.Maybe [UserStackAssociationError]) (\s a -> s {errors = a} :: BatchAssociateUserStackResponse)
{-# DEPRECATED bausrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bausrsResponseStatus :: Lens.Lens' BatchAssociateUserStackResponse Lude.Int
bausrsResponseStatus = Lens.lens (responseStatus :: BatchAssociateUserStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchAssociateUserStackResponse)
{-# DEPRECATED bausrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

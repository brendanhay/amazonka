{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteTrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a traffic policy instance and all of the resource record sets that Amazon Route 53 created when you created the instance.
module Network.AWS.Route53.DeleteTrafficPolicyInstance
  ( -- * Creating a request
    DeleteTrafficPolicyInstance (..),
    mkDeleteTrafficPolicyInstance,

    -- ** Request lenses
    dtpiId,

    -- * Destructuring the response
    DeleteTrafficPolicyInstanceResponse (..),
    mkDeleteTrafficPolicyInstanceResponse,

    -- ** Response lenses
    dtpirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to delete a specified traffic policy instance.
--
-- /See:/ 'mkDeleteTrafficPolicyInstance' smart constructor.
newtype DeleteTrafficPolicyInstance = DeleteTrafficPolicyInstance'
  { id ::
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

-- | Creates a value of 'DeleteTrafficPolicyInstance' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the traffic policy instance that you want to delete.
--
-- /Important:/ When you delete a traffic policy instance, Amazon Route 53 also deletes all of the resource record sets that were created when you created the traffic policy instance.
mkDeleteTrafficPolicyInstance ::
  -- | 'id'
  Lude.Text ->
  DeleteTrafficPolicyInstance
mkDeleteTrafficPolicyInstance pId_ =
  DeleteTrafficPolicyInstance' {id = pId_}

-- | The ID of the traffic policy instance that you want to delete.
--
-- /Important:/ When you delete a traffic policy instance, Amazon Route 53 also deletes all of the resource record sets that were created when you created the traffic policy instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpiId :: Lens.Lens' DeleteTrafficPolicyInstance Lude.Text
dtpiId = Lens.lens (id :: DeleteTrafficPolicyInstance -> Lude.Text) (\s a -> s {id = a} :: DeleteTrafficPolicyInstance)
{-# DEPRECATED dtpiId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteTrafficPolicyInstance where
  type
    Rs DeleteTrafficPolicyInstance =
      DeleteTrafficPolicyInstanceResponse
  request = Req.delete route53Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTrafficPolicyInstanceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrafficPolicyInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTrafficPolicyInstance where
  toPath DeleteTrafficPolicyInstance' {..} =
    Lude.mconcat ["/2013-04-01/trafficpolicyinstance/", Lude.toBS id]

instance Lude.ToQuery DeleteTrafficPolicyInstance where
  toQuery = Lude.const Lude.mempty

-- | An empty element.
--
-- /See:/ 'mkDeleteTrafficPolicyInstanceResponse' smart constructor.
newtype DeleteTrafficPolicyInstanceResponse = DeleteTrafficPolicyInstanceResponse'
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

-- | Creates a value of 'DeleteTrafficPolicyInstanceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTrafficPolicyInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrafficPolicyInstanceResponse
mkDeleteTrafficPolicyInstanceResponse pResponseStatus_ =
  DeleteTrafficPolicyInstanceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpirsResponseStatus :: Lens.Lens' DeleteTrafficPolicyInstanceResponse Lude.Int
dtpirsResponseStatus = Lens.lens (responseStatus :: DeleteTrafficPolicyInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrafficPolicyInstanceResponse)
{-# DEPRECATED dtpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

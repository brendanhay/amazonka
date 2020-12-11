{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteReusableDelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reusable delegation set.
--
-- /Important:/ You can delete a reusable delegation set only if it isn't associated with any hosted zones.
-- To verify that the reusable delegation set is not associated with any hosted zones, submit a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetReusableDelegationSet.html GetReusableDelegationSet> request and specify the ID of the reusable delegation set that you want to delete.
module Network.AWS.Route53.DeleteReusableDelegationSet
  ( -- * Creating a request
    DeleteReusableDelegationSet (..),
    mkDeleteReusableDelegationSet,

    -- ** Request lenses
    drdsId,

    -- * Destructuring the response
    DeleteReusableDelegationSetResponse (..),
    mkDeleteReusableDelegationSetResponse,

    -- ** Response lenses
    drdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to delete a reusable delegation set.
--
-- /See:/ 'mkDeleteReusableDelegationSet' smart constructor.
newtype DeleteReusableDelegationSet = DeleteReusableDelegationSet'
  { id ::
      ResourceId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReusableDelegationSet' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the reusable delegation set that you want to delete.
mkDeleteReusableDelegationSet ::
  -- | 'id'
  ResourceId ->
  DeleteReusableDelegationSet
mkDeleteReusableDelegationSet pId_ =
  DeleteReusableDelegationSet' {id = pId_}

-- | The ID of the reusable delegation set that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsId :: Lens.Lens' DeleteReusableDelegationSet ResourceId
drdsId = Lens.lens (id :: DeleteReusableDelegationSet -> ResourceId) (\s a -> s {id = a} :: DeleteReusableDelegationSet)
{-# DEPRECATED drdsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteReusableDelegationSet where
  type
    Rs DeleteReusableDelegationSet =
      DeleteReusableDelegationSetResponse
  request = Req.delete route53Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteReusableDelegationSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReusableDelegationSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteReusableDelegationSet where
  toPath DeleteReusableDelegationSet' {..} =
    Lude.mconcat ["/2013-04-01/delegationset/", Lude.toBS id]

instance Lude.ToQuery DeleteReusableDelegationSet where
  toQuery = Lude.const Lude.mempty

-- | An empty element.
--
-- /See:/ 'mkDeleteReusableDelegationSetResponse' smart constructor.
newtype DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse'
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

-- | Creates a value of 'DeleteReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteReusableDelegationSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReusableDelegationSetResponse
mkDeleteReusableDelegationSetResponse pResponseStatus_ =
  DeleteReusableDelegationSetResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrsResponseStatus :: Lens.Lens' DeleteReusableDelegationSetResponse Lude.Int
drdsrsResponseStatus = Lens.lens (responseStatus :: DeleteReusableDelegationSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReusableDelegationSetResponse)
{-# DEPRECATED drdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

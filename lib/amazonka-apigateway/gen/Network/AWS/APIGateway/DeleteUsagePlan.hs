{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.DeleteUsagePlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan of a given plan Id.
module Network.AWS.APIGateway.DeleteUsagePlan
  ( -- * Creating a request
    DeleteUsagePlan (..),
    mkDeleteUsagePlan,

    -- ** Request lenses
    dupUsagePlanId,

    -- * Destructuring the response
    DeleteUsagePlanResponse (..),
    mkDeleteUsagePlanResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The DELETE request to delete a usage plan of a given plan Id.
--
-- /See:/ 'mkDeleteUsagePlan' smart constructor.
newtype DeleteUsagePlan = DeleteUsagePlan'
  { usagePlanId ::
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

-- | Creates a value of 'DeleteUsagePlan' with the minimum fields required to make a request.
--
-- * 'usagePlanId' - [Required] The Id of the to-be-deleted usage plan.
mkDeleteUsagePlan ::
  -- | 'usagePlanId'
  Lude.Text ->
  DeleteUsagePlan
mkDeleteUsagePlan pUsagePlanId_ =
  DeleteUsagePlan' {usagePlanId = pUsagePlanId_}

-- | [Required] The Id of the to-be-deleted usage plan.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUsagePlanId :: Lens.Lens' DeleteUsagePlan Lude.Text
dupUsagePlanId = Lens.lens (usagePlanId :: DeleteUsagePlan -> Lude.Text) (\s a -> s {usagePlanId = a} :: DeleteUsagePlan)
{-# DEPRECATED dupUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

instance Lude.AWSRequest DeleteUsagePlan where
  type Rs DeleteUsagePlan = DeleteUsagePlanResponse
  request = Req.delete apiGatewayService
  response = Res.receiveNull DeleteUsagePlanResponse'

instance Lude.ToHeaders DeleteUsagePlan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath DeleteUsagePlan where
  toPath DeleteUsagePlan' {..} =
    Lude.mconcat ["/usageplans/", Lude.toBS usagePlanId]

instance Lude.ToQuery DeleteUsagePlan where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUsagePlanResponse' smart constructor.
data DeleteUsagePlanResponse = DeleteUsagePlanResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUsagePlanResponse' with the minimum fields required to make a request.
mkDeleteUsagePlanResponse ::
  DeleteUsagePlanResponse
mkDeleteUsagePlanResponse = DeleteUsagePlanResponse'

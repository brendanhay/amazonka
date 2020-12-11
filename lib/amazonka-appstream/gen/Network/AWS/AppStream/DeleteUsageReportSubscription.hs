{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteUsageReportSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables usage report generation.
module Network.AWS.AppStream.DeleteUsageReportSubscription
  ( -- * Creating a request
    DeleteUsageReportSubscription (..),
    mkDeleteUsageReportSubscription,

    -- * Destructuring the response
    DeleteUsageReportSubscriptionResponse (..),
    mkDeleteUsageReportSubscriptionResponse,

    -- ** Response lenses
    dursrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUsageReportSubscription' smart constructor.
data DeleteUsageReportSubscription = DeleteUsageReportSubscription'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUsageReportSubscription' with the minimum fields required to make a request.
mkDeleteUsageReportSubscription ::
  DeleteUsageReportSubscription
mkDeleteUsageReportSubscription = DeleteUsageReportSubscription'

instance Lude.AWSRequest DeleteUsageReportSubscription where
  type
    Rs DeleteUsageReportSubscription =
      DeleteUsageReportSubscriptionResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteUsageReportSubscriptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteUsageReportSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.DeleteUsageReportSubscription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUsageReportSubscription where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DeleteUsageReportSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUsageReportSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUsageReportSubscriptionResponse' smart constructor.
newtype DeleteUsageReportSubscriptionResponse = DeleteUsageReportSubscriptionResponse'
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

-- | Creates a value of 'DeleteUsageReportSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteUsageReportSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteUsageReportSubscriptionResponse
mkDeleteUsageReportSubscriptionResponse pResponseStatus_ =
  DeleteUsageReportSubscriptionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dursrsResponseStatus :: Lens.Lens' DeleteUsageReportSubscriptionResponse Lude.Int
dursrsResponseStatus = Lens.lens (responseStatus :: DeleteUsageReportSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteUsageReportSubscriptionResponse)
{-# DEPRECATED dursrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

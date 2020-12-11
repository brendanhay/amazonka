{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage limit from a cluster.
module Network.AWS.Redshift.DeleteUsageLimit
  ( -- * Creating a request
    DeleteUsageLimit (..),
    mkDeleteUsageLimit,

    -- ** Request lenses
    dulUsageLimitId,

    -- * Destructuring the response
    DeleteUsageLimitResponse (..),
    mkDeleteUsageLimitResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUsageLimit' smart constructor.
newtype DeleteUsageLimit = DeleteUsageLimit'
  { usageLimitId ::
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

-- | Creates a value of 'DeleteUsageLimit' with the minimum fields required to make a request.
--
-- * 'usageLimitId' - The identifier of the usage limit to delete.
mkDeleteUsageLimit ::
  -- | 'usageLimitId'
  Lude.Text ->
  DeleteUsageLimit
mkDeleteUsageLimit pUsageLimitId_ =
  DeleteUsageLimit' {usageLimitId = pUsageLimitId_}

-- | The identifier of the usage limit to delete.
--
-- /Note:/ Consider using 'usageLimitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulUsageLimitId :: Lens.Lens' DeleteUsageLimit Lude.Text
dulUsageLimitId = Lens.lens (usageLimitId :: DeleteUsageLimit -> Lude.Text) (\s a -> s {usageLimitId = a} :: DeleteUsageLimit)
{-# DEPRECATED dulUsageLimitId "Use generic-lens or generic-optics with 'usageLimitId' instead." #-}

instance Lude.AWSRequest DeleteUsageLimit where
  type Rs DeleteUsageLimit = DeleteUsageLimitResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteUsageLimitResponse'

instance Lude.ToHeaders DeleteUsageLimit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteUsageLimit where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUsageLimit where
  toQuery DeleteUsageLimit' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteUsageLimit" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "UsageLimitId" Lude.=: usageLimitId
      ]

-- | /See:/ 'mkDeleteUsageLimitResponse' smart constructor.
data DeleteUsageLimitResponse = DeleteUsageLimitResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUsageLimitResponse' with the minimum fields required to make a request.
mkDeleteUsageLimitResponse ::
  DeleteUsageLimitResponse
mkDeleteUsageLimitResponse = DeleteUsageLimitResponse'

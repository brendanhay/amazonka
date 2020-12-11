{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log group and permanently deletes all the archived log events associated with the log group.
module Network.AWS.CloudWatchLogs.DeleteLogGroup
  ( -- * Creating a request
    DeleteLogGroup (..),
    mkDeleteLogGroup,

    -- ** Request lenses
    dlgLogGroupName,

    -- * Destructuring the response
    DeleteLogGroupResponse (..),
    mkDeleteLogGroupResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLogGroup' smart constructor.
newtype DeleteLogGroup = DeleteLogGroup' {logGroupName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLogGroup' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
mkDeleteLogGroup ::
  -- | 'logGroupName'
  Lude.Text ->
  DeleteLogGroup
mkDeleteLogGroup pLogGroupName_ =
  DeleteLogGroup' {logGroupName = pLogGroupName_}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgLogGroupName :: Lens.Lens' DeleteLogGroup Lude.Text
dlgLogGroupName = Lens.lens (logGroupName :: DeleteLogGroup -> Lude.Text) (\s a -> s {logGroupName = a} :: DeleteLogGroup)
{-# DEPRECATED dlgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.AWSRequest DeleteLogGroup where
  type Rs DeleteLogGroup = DeleteLogGroupResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull DeleteLogGroupResponse'

instance Lude.ToHeaders DeleteLogGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DeleteLogGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLogGroup where
  toJSON DeleteLogGroup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("logGroupName" Lude..= logGroupName)])

instance Lude.ToPath DeleteLogGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLogGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLogGroupResponse' smart constructor.
data DeleteLogGroupResponse = DeleteLogGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLogGroupResponse' with the minimum fields required to make a request.
mkDeleteLogGroupResponse ::
  DeleteLogGroupResponse
mkDeleteLogGroupResponse = DeleteLogGroupResponse'

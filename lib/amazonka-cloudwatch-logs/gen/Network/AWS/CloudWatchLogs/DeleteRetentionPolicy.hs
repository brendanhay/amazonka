{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy.
--
-- Log events do not expire if they belong to log groups without a retention policy.
module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
  ( -- * Creating a request
    DeleteRetentionPolicy (..),
    mkDeleteRetentionPolicy,

    -- ** Request lenses
    drpLogGroupName,

    -- * Destructuring the response
    DeleteRetentionPolicyResponse (..),
    mkDeleteRetentionPolicyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRetentionPolicy' smart constructor.
newtype DeleteRetentionPolicy = DeleteRetentionPolicy'
  { logGroupName ::
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

-- | Creates a value of 'DeleteRetentionPolicy' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
mkDeleteRetentionPolicy ::
  -- | 'logGroupName'
  Lude.Text ->
  DeleteRetentionPolicy
mkDeleteRetentionPolicy pLogGroupName_ =
  DeleteRetentionPolicy' {logGroupName = pLogGroupName_}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpLogGroupName :: Lens.Lens' DeleteRetentionPolicy Lude.Text
drpLogGroupName = Lens.lens (logGroupName :: DeleteRetentionPolicy -> Lude.Text) (\s a -> s {logGroupName = a} :: DeleteRetentionPolicy)
{-# DEPRECATED drpLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.AWSRequest DeleteRetentionPolicy where
  type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull DeleteRetentionPolicyResponse'

instance Lude.ToHeaders DeleteRetentionPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DeleteRetentionPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRetentionPolicy where
  toJSON DeleteRetentionPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("logGroupName" Lude..= logGroupName)])

instance Lude.ToPath DeleteRetentionPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRetentionPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteRetentionPolicyResponse' smart constructor.
data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRetentionPolicyResponse' with the minimum fields required to make a request.
mkDeleteRetentionPolicyResponse ::
  DeleteRetentionPolicyResponse
mkDeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'

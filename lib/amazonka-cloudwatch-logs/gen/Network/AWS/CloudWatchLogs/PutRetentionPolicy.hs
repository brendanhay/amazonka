{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the retention of the specified log group. A retention policy allows you to configure the number of days for which to retain log events in the specified log group.
module Network.AWS.CloudWatchLogs.PutRetentionPolicy
  ( -- * Creating a request
    PutRetentionPolicy (..),
    mkPutRetentionPolicy,

    -- ** Request lenses
    prpLogGroupName,
    prpRetentionInDays,

    -- * Destructuring the response
    PutRetentionPolicyResponse (..),
    mkPutRetentionPolicyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { logGroupName ::
      Lude.Text,
    retentionInDays :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRetentionPolicy' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
-- * 'retentionInDays' - Undocumented field.
mkPutRetentionPolicy ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'retentionInDays'
  Lude.Int ->
  PutRetentionPolicy
mkPutRetentionPolicy pLogGroupName_ pRetentionInDays_ =
  PutRetentionPolicy'
    { logGroupName = pLogGroupName_,
      retentionInDays = pRetentionInDays_
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpLogGroupName :: Lens.Lens' PutRetentionPolicy Lude.Text
prpLogGroupName = Lens.lens (logGroupName :: PutRetentionPolicy -> Lude.Text) (\s a -> s {logGroupName = a} :: PutRetentionPolicy)
{-# DEPRECATED prpLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'retentionInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpRetentionInDays :: Lens.Lens' PutRetentionPolicy Lude.Int
prpRetentionInDays = Lens.lens (retentionInDays :: PutRetentionPolicy -> Lude.Int) (\s a -> s {retentionInDays = a} :: PutRetentionPolicy)
{-# DEPRECATED prpRetentionInDays "Use generic-lens or generic-optics with 'retentionInDays' instead." #-}

instance Lude.AWSRequest PutRetentionPolicy where
  type Rs PutRetentionPolicy = PutRetentionPolicyResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull PutRetentionPolicyResponse'

instance Lude.ToHeaders PutRetentionPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.PutRetentionPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("retentionInDays" Lude..= retentionInDays)
          ]
      )

instance Lude.ToPath PutRetentionPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRetentionPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRetentionPolicyResponse' with the minimum fields required to make a request.
mkPutRetentionPolicyResponse ::
  PutRetentionPolicyResponse
mkPutRetentionPolicyResponse = PutRetentionPolicyResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeleteNotificationChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Firewall Manager association with the IAM role and the Amazon Simple Notification Service (SNS) topic that is used to record AWS Firewall Manager SNS logs.
module Network.AWS.FMS.DeleteNotificationChannel
  ( -- * Creating a request
    DeleteNotificationChannel (..),
    mkDeleteNotificationChannel,

    -- * Destructuring the response
    DeleteNotificationChannelResponse (..),
    mkDeleteNotificationChannelResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteNotificationChannel' smart constructor.
data DeleteNotificationChannel = DeleteNotificationChannel'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotificationChannel' with the minimum fields required to make a request.
mkDeleteNotificationChannel ::
  DeleteNotificationChannel
mkDeleteNotificationChannel = DeleteNotificationChannel'

instance Lude.AWSRequest DeleteNotificationChannel where
  type
    Rs DeleteNotificationChannel =
      DeleteNotificationChannelResponse
  request = Req.postJSON fmsService
  response = Res.receiveNull DeleteNotificationChannelResponse'

instance Lude.ToHeaders DeleteNotificationChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.DeleteNotificationChannel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteNotificationChannel where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DeleteNotificationChannel where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNotificationChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteNotificationChannelResponse' smart constructor.
data DeleteNotificationChannelResponse = DeleteNotificationChannelResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotificationChannelResponse' with the minimum fields required to make a request.
mkDeleteNotificationChannelResponse ::
  DeleteNotificationChannelResponse
mkDeleteNotificationChannelResponse =
  DeleteNotificationChannelResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeletePlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a platform application object for one of the supported push notification services, such as APNS and GCM (Firebase Cloud Messaging). For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
module Network.AWS.SNS.DeletePlatformApplication
  ( -- * Creating a request
    DeletePlatformApplication (..),
    mkDeletePlatformApplication,

    -- ** Request lenses
    dpaPlatformApplicationARN,

    -- * Destructuring the response
    DeletePlatformApplicationResponse (..),
    mkDeletePlatformApplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for DeletePlatformApplication action.
--
-- /See:/ 'mkDeletePlatformApplication' smart constructor.
newtype DeletePlatformApplication = DeletePlatformApplication'
  { -- | PlatformApplicationArn of platform application object to delete.
    platformApplicationARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePlatformApplication' with the minimum fields required to make a request.
--
-- * 'platformApplicationARN' - PlatformApplicationArn of platform application object to delete.
mkDeletePlatformApplication ::
  -- | 'platformApplicationARN'
  Lude.Text ->
  DeletePlatformApplication
mkDeletePlatformApplication pPlatformApplicationARN_ =
  DeletePlatformApplication'
    { platformApplicationARN =
        pPlatformApplicationARN_
    }

-- | PlatformApplicationArn of platform application object to delete.
--
-- /Note:/ Consider using 'platformApplicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaPlatformApplicationARN :: Lens.Lens' DeletePlatformApplication Lude.Text
dpaPlatformApplicationARN = Lens.lens (platformApplicationARN :: DeletePlatformApplication -> Lude.Text) (\s a -> s {platformApplicationARN = a} :: DeletePlatformApplication)
{-# DEPRECATED dpaPlatformApplicationARN "Use generic-lens or generic-optics with 'platformApplicationARN' instead." #-}

instance Lude.AWSRequest DeletePlatformApplication where
  type
    Rs DeletePlatformApplication =
      DeletePlatformApplicationResponse
  request = Req.postQuery snsService
  response = Res.receiveNull DeletePlatformApplicationResponse'

instance Lude.ToHeaders DeletePlatformApplication where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePlatformApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePlatformApplication where
  toQuery DeletePlatformApplication' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeletePlatformApplication" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "PlatformApplicationArn" Lude.=: platformApplicationARN
      ]

-- | /See:/ 'mkDeletePlatformApplicationResponse' smart constructor.
data DeletePlatformApplicationResponse = DeletePlatformApplicationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePlatformApplicationResponse' with the minimum fields required to make a request.
mkDeletePlatformApplicationResponse ::
  DeletePlatformApplicationResponse
mkDeletePlatformApplicationResponse =
  DeletePlatformApplicationResponse'

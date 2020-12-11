{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableSSO
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables single-sign on for a directory.
module Network.AWS.DirectoryService.DisableSSO
  ( -- * Creating a request
    DisableSSO (..),
    mkDisableSSO,

    -- ** Request lenses
    dssoUserName,
    dssoPassword,
    dssoDirectoryId,

    -- * Destructuring the response
    DisableSSOResponse (..),
    mkDisableSSOResponse,

    -- ** Response lenses
    dssorsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DisableSso' operation.
--
-- /See:/ 'mkDisableSSO' smart constructor.
data DisableSSO = DisableSSO'
  { userName :: Lude.Maybe Lude.Text,
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    directoryId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableSSO' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which to disable single-sign on.
-- * 'password' - The password of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
-- * 'userName' - The username of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. This account must have privileges to remove a service principal name.
--
-- If the AD Connector service account does not have privileges to remove a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to disable single sign-on and are not stored by the service. The AD Connector service account is not changed.
mkDisableSSO ::
  -- | 'directoryId'
  Lude.Text ->
  DisableSSO
mkDisableSSO pDirectoryId_ =
  DisableSSO'
    { userName = Lude.Nothing,
      password = Lude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The username of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. This account must have privileges to remove a service principal name.
--
-- If the AD Connector service account does not have privileges to remove a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to disable single sign-on and are not stored by the service. The AD Connector service account is not changed.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoUserName :: Lens.Lens' DisableSSO (Lude.Maybe Lude.Text)
dssoUserName = Lens.lens (userName :: DisableSSO -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: DisableSSO)
{-# DEPRECATED dssoUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The password of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoPassword :: Lens.Lens' DisableSSO (Lude.Maybe (Lude.Sensitive Lude.Text))
dssoPassword = Lens.lens (password :: DisableSSO -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: DisableSSO)
{-# DEPRECATED dssoPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The identifier of the directory for which to disable single-sign on.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoDirectoryId :: Lens.Lens' DisableSSO Lude.Text
dssoDirectoryId = Lens.lens (directoryId :: DisableSSO -> Lude.Text) (\s a -> s {directoryId = a} :: DisableSSO)
{-# DEPRECATED dssoDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest DisableSSO where
  type Rs DisableSSO = DisableSSOResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisableSSOResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableSSO where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DisableSso" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableSSO where
  toJSON DisableSSO' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserName" Lude..=) Lude.<$> userName,
            ("Password" Lude..=) Lude.<$> password,
            Lude.Just ("DirectoryId" Lude..= directoryId)
          ]
      )

instance Lude.ToPath DisableSSO where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableSSO where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'DisableSso' operation.
--
-- /See:/ 'mkDisableSSOResponse' smart constructor.
newtype DisableSSOResponse = DisableSSOResponse'
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

-- | Creates a value of 'DisableSSOResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisableSSOResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableSSOResponse
mkDisableSSOResponse pResponseStatus_ =
  DisableSSOResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssorsResponseStatus :: Lens.Lens' DisableSSOResponse Lude.Int
dssorsResponseStatus = Lens.lens (responseStatus :: DisableSSOResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableSSOResponse)
{-# DEPRECATED dssorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

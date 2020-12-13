{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.EnableSSO
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables single sign-on for a directory. Single sign-on allows users in your directory to access certain AWS services from a computer joined to the directory without having to enter their credentials separately.
module Network.AWS.DirectoryService.EnableSSO
  ( -- * Creating a request
    EnableSSO (..),
    mkEnableSSO,

    -- ** Request lenses
    esDirectoryId,
    esUserName,
    esPassword,

    -- * Destructuring the response
    EnableSSOResponse (..),
    mkEnableSSOResponse,

    -- ** Response lenses
    esrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'EnableSso' operation.
--
-- /See:/ 'mkEnableSSO' smart constructor.
data EnableSSO = EnableSSO'
  { -- | The identifier of the directory for which to enable single-sign on.
    directoryId :: Lude.Text,
    -- | The username of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. This account must have privileges to add a service principal name.
    --
    -- If the AD Connector service account does not have privileges to add a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to enable single sign-on and are not stored by the service. The AD Connector service account is not changed.
    userName :: Lude.Maybe Lude.Text,
    -- | The password of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
    password :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableSSO' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which to enable single-sign on.
-- * 'userName' - The username of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. This account must have privileges to add a service principal name.
--
-- If the AD Connector service account does not have privileges to add a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to enable single sign-on and are not stored by the service. The AD Connector service account is not changed.
-- * 'password' - The password of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
mkEnableSSO ::
  -- | 'directoryId'
  Lude.Text ->
  EnableSSO
mkEnableSSO pDirectoryId_ =
  EnableSSO'
    { directoryId = pDirectoryId_,
      userName = Lude.Nothing,
      password = Lude.Nothing
    }

-- | The identifier of the directory for which to enable single-sign on.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDirectoryId :: Lens.Lens' EnableSSO Lude.Text
esDirectoryId = Lens.lens (directoryId :: EnableSSO -> Lude.Text) (\s a -> s {directoryId = a} :: EnableSSO)
{-# DEPRECATED esDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The username of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. This account must have privileges to add a service principal name.
--
-- If the AD Connector service account does not have privileges to add a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to enable single sign-on and are not stored by the service. The AD Connector service account is not changed.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esUserName :: Lens.Lens' EnableSSO (Lude.Maybe Lude.Text)
esUserName = Lens.lens (userName :: EnableSSO -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: EnableSSO)
{-# DEPRECATED esUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The password of an alternate account to use to enable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPassword :: Lens.Lens' EnableSSO (Lude.Maybe (Lude.Sensitive Lude.Text))
esPassword = Lens.lens (password :: EnableSSO -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: EnableSSO)
{-# DEPRECATED esPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest EnableSSO where
  type Rs EnableSSO = EnableSSOResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          EnableSSOResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableSSO where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.EnableSso" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableSSO where
  toJSON EnableSSO' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            ("UserName" Lude..=) Lude.<$> userName,
            ("Password" Lude..=) Lude.<$> password
          ]
      )

instance Lude.ToPath EnableSSO where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableSSO where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'EnableSso' operation.
--
-- /See:/ 'mkEnableSSOResponse' smart constructor.
newtype EnableSSOResponse = EnableSSOResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableSSOResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkEnableSSOResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableSSOResponse
mkEnableSSOResponse pResponseStatus_ =
  EnableSSOResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsResponseStatus :: Lens.Lens' EnableSSOResponse Lude.Int
esrsResponseStatus = Lens.lens (responseStatus :: EnableSSOResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableSSOResponse)
{-# DEPRECATED esrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.InstallToRemoteAccessSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Installs an application to the device in a remote access session. For Android applications, the file must be in .apk format. For iOS applications, the file must be in .ipa format.
module Network.AWS.DeviceFarm.InstallToRemoteAccessSession
  ( -- * Creating a request
    InstallToRemoteAccessSession (..),
    mkInstallToRemoteAccessSession,

    -- ** Request lenses
    itrasRemoteAccessSessionARN,
    itrasAppARN,

    -- * Destructuring the response
    InstallToRemoteAccessSessionResponse (..),
    mkInstallToRemoteAccessSessionResponse,

    -- ** Response lenses
    itrasrsAppUpload,
    itrasrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to install an Android application (in .apk format) or an iOS application (in .ipa format) as part of a remote access session.
--
-- /See:/ 'mkInstallToRemoteAccessSession' smart constructor.
data InstallToRemoteAccessSession = InstallToRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
    remoteAccessSessionARN :: Lude.Text,
    -- | The ARN of the app about which you are requesting information.
    appARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstallToRemoteAccessSession' with the minimum fields required to make a request.
--
-- * 'remoteAccessSessionARN' - The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
-- * 'appARN' - The ARN of the app about which you are requesting information.
mkInstallToRemoteAccessSession ::
  -- | 'remoteAccessSessionARN'
  Lude.Text ->
  -- | 'appARN'
  Lude.Text ->
  InstallToRemoteAccessSession
mkInstallToRemoteAccessSession pRemoteAccessSessionARN_ pAppARN_ =
  InstallToRemoteAccessSession'
    { remoteAccessSessionARN =
        pRemoteAccessSessionARN_,
      appARN = pAppARN_
    }

-- | The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
--
-- /Note:/ Consider using 'remoteAccessSessionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrasRemoteAccessSessionARN :: Lens.Lens' InstallToRemoteAccessSession Lude.Text
itrasRemoteAccessSessionARN = Lens.lens (remoteAccessSessionARN :: InstallToRemoteAccessSession -> Lude.Text) (\s a -> s {remoteAccessSessionARN = a} :: InstallToRemoteAccessSession)
{-# DEPRECATED itrasRemoteAccessSessionARN "Use generic-lens or generic-optics with 'remoteAccessSessionARN' instead." #-}

-- | The ARN of the app about which you are requesting information.
--
-- /Note:/ Consider using 'appARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrasAppARN :: Lens.Lens' InstallToRemoteAccessSession Lude.Text
itrasAppARN = Lens.lens (appARN :: InstallToRemoteAccessSession -> Lude.Text) (\s a -> s {appARN = a} :: InstallToRemoteAccessSession)
{-# DEPRECATED itrasAppARN "Use generic-lens or generic-optics with 'appARN' instead." #-}

instance Lude.AWSRequest InstallToRemoteAccessSession where
  type
    Rs InstallToRemoteAccessSession =
      InstallToRemoteAccessSessionResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          InstallToRemoteAccessSessionResponse'
            Lude.<$> (x Lude..?> "appUpload") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InstallToRemoteAccessSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DeviceFarm_20150623.InstallToRemoteAccessSession" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON InstallToRemoteAccessSession where
  toJSON InstallToRemoteAccessSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("remoteAccessSessionArn" Lude..= remoteAccessSessionARN),
            Lude.Just ("appArn" Lude..= appARN)
          ]
      )

instance Lude.ToPath InstallToRemoteAccessSession where
  toPath = Lude.const "/"

instance Lude.ToQuery InstallToRemoteAccessSession where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server after AWS Device Farm makes a request to install to a remote access session.
--
-- /See:/ 'mkInstallToRemoteAccessSessionResponse' smart constructor.
data InstallToRemoteAccessSessionResponse = InstallToRemoteAccessSessionResponse'
  { -- | An app to upload or that has been uploaded.
    appUpload :: Lude.Maybe Upload,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstallToRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- * 'appUpload' - An app to upload or that has been uploaded.
-- * 'responseStatus' - The response status code.
mkInstallToRemoteAccessSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InstallToRemoteAccessSessionResponse
mkInstallToRemoteAccessSessionResponse pResponseStatus_ =
  InstallToRemoteAccessSessionResponse'
    { appUpload = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An app to upload or that has been uploaded.
--
-- /Note:/ Consider using 'appUpload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrasrsAppUpload :: Lens.Lens' InstallToRemoteAccessSessionResponse (Lude.Maybe Upload)
itrasrsAppUpload = Lens.lens (appUpload :: InstallToRemoteAccessSessionResponse -> Lude.Maybe Upload) (\s a -> s {appUpload = a} :: InstallToRemoteAccessSessionResponse)
{-# DEPRECATED itrasrsAppUpload "Use generic-lens or generic-optics with 'appUpload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itrasrsResponseStatus :: Lens.Lens' InstallToRemoteAccessSessionResponse Lude.Int
itrasrsResponseStatus = Lens.lens (responseStatus :: InstallToRemoteAccessSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InstallToRemoteAccessSessionResponse)
{-# DEPRECATED itrasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

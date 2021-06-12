{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.InstallToRemoteAccessSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Installs an application to the device in a remote access session. For
-- Android applications, the file must be in .apk format. For iOS
-- applications, the file must be in .ipa format.
module Network.AWS.DeviceFarm.InstallToRemoteAccessSession
  ( -- * Creating a Request
    InstallToRemoteAccessSession (..),
    newInstallToRemoteAccessSession,

    -- * Request Lenses
    installToRemoteAccessSession_remoteAccessSessionArn,
    installToRemoteAccessSession_appArn,

    -- * Destructuring the Response
    InstallToRemoteAccessSessionResponse (..),
    newInstallToRemoteAccessSessionResponse,

    -- * Response Lenses
    installToRemoteAccessSessionResponse_appUpload,
    installToRemoteAccessSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to install an Android application (in .apk
-- format) or an iOS application (in .ipa format) as part of a remote
-- access session.
--
-- /See:/ 'newInstallToRemoteAccessSession' smart constructor.
data InstallToRemoteAccessSession = InstallToRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session about which
    -- you are requesting information.
    remoteAccessSessionArn :: Core.Text,
    -- | The ARN of the app about which you are requesting information.
    appArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstallToRemoteAccessSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteAccessSessionArn', 'installToRemoteAccessSession_remoteAccessSessionArn' - The Amazon Resource Name (ARN) of the remote access session about which
-- you are requesting information.
--
-- 'appArn', 'installToRemoteAccessSession_appArn' - The ARN of the app about which you are requesting information.
newInstallToRemoteAccessSession ::
  -- | 'remoteAccessSessionArn'
  Core.Text ->
  -- | 'appArn'
  Core.Text ->
  InstallToRemoteAccessSession
newInstallToRemoteAccessSession
  pRemoteAccessSessionArn_
  pAppArn_ =
    InstallToRemoteAccessSession'
      { remoteAccessSessionArn =
          pRemoteAccessSessionArn_,
        appArn = pAppArn_
      }

-- | The Amazon Resource Name (ARN) of the remote access session about which
-- you are requesting information.
installToRemoteAccessSession_remoteAccessSessionArn :: Lens.Lens' InstallToRemoteAccessSession Core.Text
installToRemoteAccessSession_remoteAccessSessionArn = Lens.lens (\InstallToRemoteAccessSession' {remoteAccessSessionArn} -> remoteAccessSessionArn) (\s@InstallToRemoteAccessSession' {} a -> s {remoteAccessSessionArn = a} :: InstallToRemoteAccessSession)

-- | The ARN of the app about which you are requesting information.
installToRemoteAccessSession_appArn :: Lens.Lens' InstallToRemoteAccessSession Core.Text
installToRemoteAccessSession_appArn = Lens.lens (\InstallToRemoteAccessSession' {appArn} -> appArn) (\s@InstallToRemoteAccessSession' {} a -> s {appArn = a} :: InstallToRemoteAccessSession)

instance Core.AWSRequest InstallToRemoteAccessSession where
  type
    AWSResponse InstallToRemoteAccessSession =
      InstallToRemoteAccessSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          InstallToRemoteAccessSessionResponse'
            Core.<$> (x Core..?> "appUpload")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable InstallToRemoteAccessSession

instance Core.NFData InstallToRemoteAccessSession

instance Core.ToHeaders InstallToRemoteAccessSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.InstallToRemoteAccessSession" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON InstallToRemoteAccessSession where
  toJSON InstallToRemoteAccessSession' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "remoteAccessSessionArn"
                  Core..= remoteAccessSessionArn
              ),
            Core.Just ("appArn" Core..= appArn)
          ]
      )

instance Core.ToPath InstallToRemoteAccessSession where
  toPath = Core.const "/"

instance Core.ToQuery InstallToRemoteAccessSession where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server after AWS Device Farm makes a
-- request to install to a remote access session.
--
-- /See:/ 'newInstallToRemoteAccessSessionResponse' smart constructor.
data InstallToRemoteAccessSessionResponse = InstallToRemoteAccessSessionResponse'
  { -- | An app to upload or that has been uploaded.
    appUpload :: Core.Maybe Upload,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstallToRemoteAccessSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appUpload', 'installToRemoteAccessSessionResponse_appUpload' - An app to upload or that has been uploaded.
--
-- 'httpStatus', 'installToRemoteAccessSessionResponse_httpStatus' - The response's http status code.
newInstallToRemoteAccessSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  InstallToRemoteAccessSessionResponse
newInstallToRemoteAccessSessionResponse pHttpStatus_ =
  InstallToRemoteAccessSessionResponse'
    { appUpload =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An app to upload or that has been uploaded.
installToRemoteAccessSessionResponse_appUpload :: Lens.Lens' InstallToRemoteAccessSessionResponse (Core.Maybe Upload)
installToRemoteAccessSessionResponse_appUpload = Lens.lens (\InstallToRemoteAccessSessionResponse' {appUpload} -> appUpload) (\s@InstallToRemoteAccessSessionResponse' {} a -> s {appUpload = a} :: InstallToRemoteAccessSessionResponse)

-- | The response's http status code.
installToRemoteAccessSessionResponse_httpStatus :: Lens.Lens' InstallToRemoteAccessSessionResponse Core.Int
installToRemoteAccessSessionResponse_httpStatus = Lens.lens (\InstallToRemoteAccessSessionResponse' {httpStatus} -> httpStatus) (\s@InstallToRemoteAccessSessionResponse' {} a -> s {httpStatus = a} :: InstallToRemoteAccessSessionResponse)

instance
  Core.NFData
    InstallToRemoteAccessSessionResponse

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
-- Module      : Amazonka.DeviceFarm.InstallToRemoteAccessSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Installs an application to the device in a remote access session. For
-- Android applications, the file must be in .apk format. For iOS
-- applications, the file must be in .ipa format.
module Amazonka.DeviceFarm.InstallToRemoteAccessSession
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to install an Android application (in .apk
-- format) or an iOS application (in .ipa format) as part of a remote
-- access session.
--
-- /See:/ 'newInstallToRemoteAccessSession' smart constructor.
data InstallToRemoteAccessSession = InstallToRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the remote access session about which
    -- you are requesting information.
    remoteAccessSessionArn :: Prelude.Text,
    -- | The ARN of the app about which you are requesting information.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'appArn'
  Prelude.Text ->
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
installToRemoteAccessSession_remoteAccessSessionArn :: Lens.Lens' InstallToRemoteAccessSession Prelude.Text
installToRemoteAccessSession_remoteAccessSessionArn = Lens.lens (\InstallToRemoteAccessSession' {remoteAccessSessionArn} -> remoteAccessSessionArn) (\s@InstallToRemoteAccessSession' {} a -> s {remoteAccessSessionArn = a} :: InstallToRemoteAccessSession)

-- | The ARN of the app about which you are requesting information.
installToRemoteAccessSession_appArn :: Lens.Lens' InstallToRemoteAccessSession Prelude.Text
installToRemoteAccessSession_appArn = Lens.lens (\InstallToRemoteAccessSession' {appArn} -> appArn) (\s@InstallToRemoteAccessSession' {} a -> s {appArn = a} :: InstallToRemoteAccessSession)

instance Core.AWSRequest InstallToRemoteAccessSession where
  type
    AWSResponse InstallToRemoteAccessSession =
      InstallToRemoteAccessSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InstallToRemoteAccessSessionResponse'
            Prelude.<$> (x Data..?> "appUpload")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    InstallToRemoteAccessSession
  where
  hashWithSalt _salt InstallToRemoteAccessSession' {..} =
    _salt
      `Prelude.hashWithSalt` remoteAccessSessionArn
      `Prelude.hashWithSalt` appArn

instance Prelude.NFData InstallToRemoteAccessSession where
  rnf InstallToRemoteAccessSession' {..} =
    Prelude.rnf remoteAccessSessionArn
      `Prelude.seq` Prelude.rnf appArn

instance Data.ToHeaders InstallToRemoteAccessSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.InstallToRemoteAccessSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InstallToRemoteAccessSession where
  toJSON InstallToRemoteAccessSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "remoteAccessSessionArn"
                  Data..= remoteAccessSessionArn
              ),
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath InstallToRemoteAccessSession where
  toPath = Prelude.const "/"

instance Data.ToQuery InstallToRemoteAccessSession where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server after AWS Device Farm makes a
-- request to install to a remote access session.
--
-- /See:/ 'newInstallToRemoteAccessSessionResponse' smart constructor.
data InstallToRemoteAccessSessionResponse = InstallToRemoteAccessSessionResponse'
  { -- | An app to upload or that has been uploaded.
    appUpload :: Prelude.Maybe Upload,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  InstallToRemoteAccessSessionResponse
newInstallToRemoteAccessSessionResponse pHttpStatus_ =
  InstallToRemoteAccessSessionResponse'
    { appUpload =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An app to upload or that has been uploaded.
installToRemoteAccessSessionResponse_appUpload :: Lens.Lens' InstallToRemoteAccessSessionResponse (Prelude.Maybe Upload)
installToRemoteAccessSessionResponse_appUpload = Lens.lens (\InstallToRemoteAccessSessionResponse' {appUpload} -> appUpload) (\s@InstallToRemoteAccessSessionResponse' {} a -> s {appUpload = a} :: InstallToRemoteAccessSessionResponse)

-- | The response's http status code.
installToRemoteAccessSessionResponse_httpStatus :: Lens.Lens' InstallToRemoteAccessSessionResponse Prelude.Int
installToRemoteAccessSessionResponse_httpStatus = Lens.lens (\InstallToRemoteAccessSessionResponse' {httpStatus} -> httpStatus) (\s@InstallToRemoteAccessSessionResponse' {} a -> s {httpStatus = a} :: InstallToRemoteAccessSessionResponse)

instance
  Prelude.NFData
    InstallToRemoteAccessSessionResponse
  where
  rnf InstallToRemoteAccessSessionResponse' {..} =
    Prelude.rnf appUpload
      `Prelude.seq` Prelude.rnf httpStatus

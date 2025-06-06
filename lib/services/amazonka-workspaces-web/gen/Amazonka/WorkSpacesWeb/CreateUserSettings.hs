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
-- Module      : Amazonka.WorkSpacesWeb.CreateUserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user settings resource that can be associated with a web
-- portal. Once associated with a web portal, user settings control how
-- users can transfer data between a streaming session and the their local
-- devices.
module Amazonka.WorkSpacesWeb.CreateUserSettings
  ( -- * Creating a Request
    CreateUserSettings (..),
    newCreateUserSettings,

    -- * Request Lenses
    createUserSettings_clientToken,
    createUserSettings_disconnectTimeoutInMinutes,
    createUserSettings_idleDisconnectTimeoutInMinutes,
    createUserSettings_tags,
    createUserSettings_copyAllowed,
    createUserSettings_downloadAllowed,
    createUserSettings_pasteAllowed,
    createUserSettings_printAllowed,
    createUserSettings_uploadAllowed,

    -- * Destructuring the Response
    CreateUserSettingsResponse (..),
    newCreateUserSettingsResponse,

    -- * Response Lenses
    createUserSettingsResponse_httpStatus,
    createUserSettingsResponse_userSettingsArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newCreateUserSettings' smart constructor.
data CreateUserSettings = CreateUserSettings'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Idempotency ensures that an API request
    -- completes only once. With an idempotent request, if the original request
    -- completes successfully, subsequent retries with the same client token
    -- returns the result from the original successful request.
    --
    -- If you do not specify a client token, one is automatically generated by
    -- the AWS SDK.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that a streaming session remains active after users
    -- disconnect.
    disconnectTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The amount of time that users can be idle (inactive) before they are
    -- disconnected from their streaming session and the disconnect timeout
    -- interval begins.
    idleDisconnectTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The tags to add to the user settings resource. A tag is a key-value
    -- pair.
    tags :: Prelude.Maybe [Data.Sensitive Tag],
    -- | Specifies whether the user can copy text from the streaming session to
    -- the local device.
    copyAllowed :: EnabledType,
    -- | Specifies whether the user can download files from the streaming session
    -- to the local device.
    downloadAllowed :: EnabledType,
    -- | Specifies whether the user can paste text from the local device to the
    -- streaming session.
    pasteAllowed :: EnabledType,
    -- | Specifies whether the user can print to the local device.
    printAllowed :: EnabledType,
    -- | Specifies whether the user can upload files from the local device to the
    -- streaming session.
    uploadAllowed :: EnabledType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createUserSettings_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Idempotency ensures that an API request
-- completes only once. With an idempotent request, if the original request
-- completes successfully, subsequent retries with the same client token
-- returns the result from the original successful request.
--
-- If you do not specify a client token, one is automatically generated by
-- the AWS SDK.
--
-- 'disconnectTimeoutInMinutes', 'createUserSettings_disconnectTimeoutInMinutes' - The amount of time that a streaming session remains active after users
-- disconnect.
--
-- 'idleDisconnectTimeoutInMinutes', 'createUserSettings_idleDisconnectTimeoutInMinutes' - The amount of time that users can be idle (inactive) before they are
-- disconnected from their streaming session and the disconnect timeout
-- interval begins.
--
-- 'tags', 'createUserSettings_tags' - The tags to add to the user settings resource. A tag is a key-value
-- pair.
--
-- 'copyAllowed', 'createUserSettings_copyAllowed' - Specifies whether the user can copy text from the streaming session to
-- the local device.
--
-- 'downloadAllowed', 'createUserSettings_downloadAllowed' - Specifies whether the user can download files from the streaming session
-- to the local device.
--
-- 'pasteAllowed', 'createUserSettings_pasteAllowed' - Specifies whether the user can paste text from the local device to the
-- streaming session.
--
-- 'printAllowed', 'createUserSettings_printAllowed' - Specifies whether the user can print to the local device.
--
-- 'uploadAllowed', 'createUserSettings_uploadAllowed' - Specifies whether the user can upload files from the local device to the
-- streaming session.
newCreateUserSettings ::
  -- | 'copyAllowed'
  EnabledType ->
  -- | 'downloadAllowed'
  EnabledType ->
  -- | 'pasteAllowed'
  EnabledType ->
  -- | 'printAllowed'
  EnabledType ->
  -- | 'uploadAllowed'
  EnabledType ->
  CreateUserSettings
newCreateUserSettings
  pCopyAllowed_
  pDownloadAllowed_
  pPasteAllowed_
  pPrintAllowed_
  pUploadAllowed_ =
    CreateUserSettings'
      { clientToken = Prelude.Nothing,
        disconnectTimeoutInMinutes = Prelude.Nothing,
        idleDisconnectTimeoutInMinutes = Prelude.Nothing,
        tags = Prelude.Nothing,
        copyAllowed = pCopyAllowed_,
        downloadAllowed = pDownloadAllowed_,
        pasteAllowed = pPasteAllowed_,
        printAllowed = pPrintAllowed_,
        uploadAllowed = pUploadAllowed_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Idempotency ensures that an API request
-- completes only once. With an idempotent request, if the original request
-- completes successfully, subsequent retries with the same client token
-- returns the result from the original successful request.
--
-- If you do not specify a client token, one is automatically generated by
-- the AWS SDK.
createUserSettings_clientToken :: Lens.Lens' CreateUserSettings (Prelude.Maybe Prelude.Text)
createUserSettings_clientToken = Lens.lens (\CreateUserSettings' {clientToken} -> clientToken) (\s@CreateUserSettings' {} a -> s {clientToken = a} :: CreateUserSettings)

-- | The amount of time that a streaming session remains active after users
-- disconnect.
createUserSettings_disconnectTimeoutInMinutes :: Lens.Lens' CreateUserSettings (Prelude.Maybe Prelude.Natural)
createUserSettings_disconnectTimeoutInMinutes = Lens.lens (\CreateUserSettings' {disconnectTimeoutInMinutes} -> disconnectTimeoutInMinutes) (\s@CreateUserSettings' {} a -> s {disconnectTimeoutInMinutes = a} :: CreateUserSettings)

-- | The amount of time that users can be idle (inactive) before they are
-- disconnected from their streaming session and the disconnect timeout
-- interval begins.
createUserSettings_idleDisconnectTimeoutInMinutes :: Lens.Lens' CreateUserSettings (Prelude.Maybe Prelude.Natural)
createUserSettings_idleDisconnectTimeoutInMinutes = Lens.lens (\CreateUserSettings' {idleDisconnectTimeoutInMinutes} -> idleDisconnectTimeoutInMinutes) (\s@CreateUserSettings' {} a -> s {idleDisconnectTimeoutInMinutes = a} :: CreateUserSettings)

-- | The tags to add to the user settings resource. A tag is a key-value
-- pair.
createUserSettings_tags :: Lens.Lens' CreateUserSettings (Prelude.Maybe [Tag])
createUserSettings_tags = Lens.lens (\CreateUserSettings' {tags} -> tags) (\s@CreateUserSettings' {} a -> s {tags = a} :: CreateUserSettings) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the user can copy text from the streaming session to
-- the local device.
createUserSettings_copyAllowed :: Lens.Lens' CreateUserSettings EnabledType
createUserSettings_copyAllowed = Lens.lens (\CreateUserSettings' {copyAllowed} -> copyAllowed) (\s@CreateUserSettings' {} a -> s {copyAllowed = a} :: CreateUserSettings)

-- | Specifies whether the user can download files from the streaming session
-- to the local device.
createUserSettings_downloadAllowed :: Lens.Lens' CreateUserSettings EnabledType
createUserSettings_downloadAllowed = Lens.lens (\CreateUserSettings' {downloadAllowed} -> downloadAllowed) (\s@CreateUserSettings' {} a -> s {downloadAllowed = a} :: CreateUserSettings)

-- | Specifies whether the user can paste text from the local device to the
-- streaming session.
createUserSettings_pasteAllowed :: Lens.Lens' CreateUserSettings EnabledType
createUserSettings_pasteAllowed = Lens.lens (\CreateUserSettings' {pasteAllowed} -> pasteAllowed) (\s@CreateUserSettings' {} a -> s {pasteAllowed = a} :: CreateUserSettings)

-- | Specifies whether the user can print to the local device.
createUserSettings_printAllowed :: Lens.Lens' CreateUserSettings EnabledType
createUserSettings_printAllowed = Lens.lens (\CreateUserSettings' {printAllowed} -> printAllowed) (\s@CreateUserSettings' {} a -> s {printAllowed = a} :: CreateUserSettings)

-- | Specifies whether the user can upload files from the local device to the
-- streaming session.
createUserSettings_uploadAllowed :: Lens.Lens' CreateUserSettings EnabledType
createUserSettings_uploadAllowed = Lens.lens (\CreateUserSettings' {uploadAllowed} -> uploadAllowed) (\s@CreateUserSettings' {} a -> s {uploadAllowed = a} :: CreateUserSettings)

instance Core.AWSRequest CreateUserSettings where
  type
    AWSResponse CreateUserSettings =
      CreateUserSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "userSettingsArn")
      )

instance Prelude.Hashable CreateUserSettings where
  hashWithSalt _salt CreateUserSettings' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` disconnectTimeoutInMinutes
      `Prelude.hashWithSalt` idleDisconnectTimeoutInMinutes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` copyAllowed
      `Prelude.hashWithSalt` downloadAllowed
      `Prelude.hashWithSalt` pasteAllowed
      `Prelude.hashWithSalt` printAllowed
      `Prelude.hashWithSalt` uploadAllowed

instance Prelude.NFData CreateUserSettings where
  rnf CreateUserSettings' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf disconnectTimeoutInMinutes `Prelude.seq`
        Prelude.rnf idleDisconnectTimeoutInMinutes `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf copyAllowed `Prelude.seq`
              Prelude.rnf downloadAllowed `Prelude.seq`
                Prelude.rnf pasteAllowed `Prelude.seq`
                  Prelude.rnf printAllowed `Prelude.seq`
                    Prelude.rnf uploadAllowed

instance Data.ToHeaders CreateUserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUserSettings where
  toJSON CreateUserSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("disconnectTimeoutInMinutes" Data..=)
              Prelude.<$> disconnectTimeoutInMinutes,
            ("idleDisconnectTimeoutInMinutes" Data..=)
              Prelude.<$> idleDisconnectTimeoutInMinutes,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("copyAllowed" Data..= copyAllowed),
            Prelude.Just
              ("downloadAllowed" Data..= downloadAllowed),
            Prelude.Just ("pasteAllowed" Data..= pasteAllowed),
            Prelude.Just ("printAllowed" Data..= printAllowed),
            Prelude.Just
              ("uploadAllowed" Data..= uploadAllowed)
          ]
      )

instance Data.ToPath CreateUserSettings where
  toPath = Prelude.const "/userSettings"

instance Data.ToQuery CreateUserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserSettingsResponse' smart constructor.
data CreateUserSettingsResponse = CreateUserSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the user settings.
    userSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createUserSettingsResponse_httpStatus' - The response's http status code.
--
-- 'userSettingsArn', 'createUserSettingsResponse_userSettingsArn' - The ARN of the user settings.
newCreateUserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'userSettingsArn'
  Prelude.Text ->
  CreateUserSettingsResponse
newCreateUserSettingsResponse
  pHttpStatus_
  pUserSettingsArn_ =
    CreateUserSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        userSettingsArn = pUserSettingsArn_
      }

-- | The response's http status code.
createUserSettingsResponse_httpStatus :: Lens.Lens' CreateUserSettingsResponse Prelude.Int
createUserSettingsResponse_httpStatus = Lens.lens (\CreateUserSettingsResponse' {httpStatus} -> httpStatus) (\s@CreateUserSettingsResponse' {} a -> s {httpStatus = a} :: CreateUserSettingsResponse)

-- | The ARN of the user settings.
createUserSettingsResponse_userSettingsArn :: Lens.Lens' CreateUserSettingsResponse Prelude.Text
createUserSettingsResponse_userSettingsArn = Lens.lens (\CreateUserSettingsResponse' {userSettingsArn} -> userSettingsArn) (\s@CreateUserSettingsResponse' {} a -> s {userSettingsArn = a} :: CreateUserSettingsResponse)

instance Prelude.NFData CreateUserSettingsResponse where
  rnf CreateUserSettingsResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf userSettingsArn

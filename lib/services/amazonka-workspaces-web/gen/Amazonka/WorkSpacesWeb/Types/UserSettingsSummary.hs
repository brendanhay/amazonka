{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkSpacesWeb.Types.UserSettingsSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.UserSettingsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpacesWeb.Types.EnabledType

-- | The summary of user settings.
--
-- /See:/ 'newUserSettingsSummary' smart constructor.
data UserSettingsSummary = UserSettingsSummary'
  { -- | Specifies whether the user can copy text from the streaming session to
    -- the local device.
    copyAllowed :: Prelude.Maybe EnabledType,
    -- | The amount of time that a streaming session remains active after users
    -- disconnect.
    disconnectTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether the user can download files from the streaming session
    -- to the local device.
    downloadAllowed :: Prelude.Maybe EnabledType,
    -- | The amount of time that users can be idle (inactive) before they are
    -- disconnected from their streaming session and the disconnect timeout
    -- interval begins.
    idleDisconnectTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether the user can paste text from the local device to the
    -- streaming session.
    pasteAllowed :: Prelude.Maybe EnabledType,
    -- | Specifies whether the user can print to the local device.
    printAllowed :: Prelude.Maybe EnabledType,
    -- | Specifies whether the user can upload files from the local device to the
    -- streaming session.
    uploadAllowed :: Prelude.Maybe EnabledType,
    -- | The ARN of the user settings.
    userSettingsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserSettingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyAllowed', 'userSettingsSummary_copyAllowed' - Specifies whether the user can copy text from the streaming session to
-- the local device.
--
-- 'disconnectTimeoutInMinutes', 'userSettingsSummary_disconnectTimeoutInMinutes' - The amount of time that a streaming session remains active after users
-- disconnect.
--
-- 'downloadAllowed', 'userSettingsSummary_downloadAllowed' - Specifies whether the user can download files from the streaming session
-- to the local device.
--
-- 'idleDisconnectTimeoutInMinutes', 'userSettingsSummary_idleDisconnectTimeoutInMinutes' - The amount of time that users can be idle (inactive) before they are
-- disconnected from their streaming session and the disconnect timeout
-- interval begins.
--
-- 'pasteAllowed', 'userSettingsSummary_pasteAllowed' - Specifies whether the user can paste text from the local device to the
-- streaming session.
--
-- 'printAllowed', 'userSettingsSummary_printAllowed' - Specifies whether the user can print to the local device.
--
-- 'uploadAllowed', 'userSettingsSummary_uploadAllowed' - Specifies whether the user can upload files from the local device to the
-- streaming session.
--
-- 'userSettingsArn', 'userSettingsSummary_userSettingsArn' - The ARN of the user settings.
newUserSettingsSummary ::
  UserSettingsSummary
newUserSettingsSummary =
  UserSettingsSummary'
    { copyAllowed = Prelude.Nothing,
      disconnectTimeoutInMinutes = Prelude.Nothing,
      downloadAllowed = Prelude.Nothing,
      idleDisconnectTimeoutInMinutes = Prelude.Nothing,
      pasteAllowed = Prelude.Nothing,
      printAllowed = Prelude.Nothing,
      uploadAllowed = Prelude.Nothing,
      userSettingsArn = Prelude.Nothing
    }

-- | Specifies whether the user can copy text from the streaming session to
-- the local device.
userSettingsSummary_copyAllowed :: Lens.Lens' UserSettingsSummary (Prelude.Maybe EnabledType)
userSettingsSummary_copyAllowed = Lens.lens (\UserSettingsSummary' {copyAllowed} -> copyAllowed) (\s@UserSettingsSummary' {} a -> s {copyAllowed = a} :: UserSettingsSummary)

-- | The amount of time that a streaming session remains active after users
-- disconnect.
userSettingsSummary_disconnectTimeoutInMinutes :: Lens.Lens' UserSettingsSummary (Prelude.Maybe Prelude.Natural)
userSettingsSummary_disconnectTimeoutInMinutes = Lens.lens (\UserSettingsSummary' {disconnectTimeoutInMinutes} -> disconnectTimeoutInMinutes) (\s@UserSettingsSummary' {} a -> s {disconnectTimeoutInMinutes = a} :: UserSettingsSummary)

-- | Specifies whether the user can download files from the streaming session
-- to the local device.
userSettingsSummary_downloadAllowed :: Lens.Lens' UserSettingsSummary (Prelude.Maybe EnabledType)
userSettingsSummary_downloadAllowed = Lens.lens (\UserSettingsSummary' {downloadAllowed} -> downloadAllowed) (\s@UserSettingsSummary' {} a -> s {downloadAllowed = a} :: UserSettingsSummary)

-- | The amount of time that users can be idle (inactive) before they are
-- disconnected from their streaming session and the disconnect timeout
-- interval begins.
userSettingsSummary_idleDisconnectTimeoutInMinutes :: Lens.Lens' UserSettingsSummary (Prelude.Maybe Prelude.Natural)
userSettingsSummary_idleDisconnectTimeoutInMinutes = Lens.lens (\UserSettingsSummary' {idleDisconnectTimeoutInMinutes} -> idleDisconnectTimeoutInMinutes) (\s@UserSettingsSummary' {} a -> s {idleDisconnectTimeoutInMinutes = a} :: UserSettingsSummary)

-- | Specifies whether the user can paste text from the local device to the
-- streaming session.
userSettingsSummary_pasteAllowed :: Lens.Lens' UserSettingsSummary (Prelude.Maybe EnabledType)
userSettingsSummary_pasteAllowed = Lens.lens (\UserSettingsSummary' {pasteAllowed} -> pasteAllowed) (\s@UserSettingsSummary' {} a -> s {pasteAllowed = a} :: UserSettingsSummary)

-- | Specifies whether the user can print to the local device.
userSettingsSummary_printAllowed :: Lens.Lens' UserSettingsSummary (Prelude.Maybe EnabledType)
userSettingsSummary_printAllowed = Lens.lens (\UserSettingsSummary' {printAllowed} -> printAllowed) (\s@UserSettingsSummary' {} a -> s {printAllowed = a} :: UserSettingsSummary)

-- | Specifies whether the user can upload files from the local device to the
-- streaming session.
userSettingsSummary_uploadAllowed :: Lens.Lens' UserSettingsSummary (Prelude.Maybe EnabledType)
userSettingsSummary_uploadAllowed = Lens.lens (\UserSettingsSummary' {uploadAllowed} -> uploadAllowed) (\s@UserSettingsSummary' {} a -> s {uploadAllowed = a} :: UserSettingsSummary)

-- | The ARN of the user settings.
userSettingsSummary_userSettingsArn :: Lens.Lens' UserSettingsSummary (Prelude.Maybe Prelude.Text)
userSettingsSummary_userSettingsArn = Lens.lens (\UserSettingsSummary' {userSettingsArn} -> userSettingsArn) (\s@UserSettingsSummary' {} a -> s {userSettingsArn = a} :: UserSettingsSummary)

instance Data.FromJSON UserSettingsSummary where
  parseJSON =
    Data.withObject
      "UserSettingsSummary"
      ( \x ->
          UserSettingsSummary'
            Prelude.<$> (x Data..:? "copyAllowed")
            Prelude.<*> (x Data..:? "disconnectTimeoutInMinutes")
            Prelude.<*> (x Data..:? "downloadAllowed")
            Prelude.<*> (x Data..:? "idleDisconnectTimeoutInMinutes")
            Prelude.<*> (x Data..:? "pasteAllowed")
            Prelude.<*> (x Data..:? "printAllowed")
            Prelude.<*> (x Data..:? "uploadAllowed")
            Prelude.<*> (x Data..:? "userSettingsArn")
      )

instance Prelude.Hashable UserSettingsSummary where
  hashWithSalt _salt UserSettingsSummary' {..} =
    _salt `Prelude.hashWithSalt` copyAllowed
      `Prelude.hashWithSalt` disconnectTimeoutInMinutes
      `Prelude.hashWithSalt` downloadAllowed
      `Prelude.hashWithSalt` idleDisconnectTimeoutInMinutes
      `Prelude.hashWithSalt` pasteAllowed
      `Prelude.hashWithSalt` printAllowed
      `Prelude.hashWithSalt` uploadAllowed
      `Prelude.hashWithSalt` userSettingsArn

instance Prelude.NFData UserSettingsSummary where
  rnf UserSettingsSummary' {..} =
    Prelude.rnf copyAllowed
      `Prelude.seq` Prelude.rnf disconnectTimeoutInMinutes
      `Prelude.seq` Prelude.rnf downloadAllowed
      `Prelude.seq` Prelude.rnf idleDisconnectTimeoutInMinutes
      `Prelude.seq` Prelude.rnf pasteAllowed
      `Prelude.seq` Prelude.rnf printAllowed
      `Prelude.seq` Prelude.rnf uploadAllowed
      `Prelude.seq` Prelude.rnf userSettingsArn

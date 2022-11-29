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
-- Module      : Amazonka.WorkSpacesWeb.Types.UserAccessLoggingSettingsSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.UserAccessLoggingSettingsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary of user access logging settings.
--
-- /See:/ 'newUserAccessLoggingSettingsSummary' smart constructor.
data UserAccessLoggingSettingsSummary = UserAccessLoggingSettingsSummary'
  { -- | The ARN of the Kinesis stream.
    kinesisStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user access logging settings.
    userAccessLoggingSettingsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserAccessLoggingSettingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisStreamArn', 'userAccessLoggingSettingsSummary_kinesisStreamArn' - The ARN of the Kinesis stream.
--
-- 'userAccessLoggingSettingsArn', 'userAccessLoggingSettingsSummary_userAccessLoggingSettingsArn' - The ARN of the user access logging settings.
newUserAccessLoggingSettingsSummary ::
  UserAccessLoggingSettingsSummary
newUserAccessLoggingSettingsSummary =
  UserAccessLoggingSettingsSummary'
    { kinesisStreamArn =
        Prelude.Nothing,
      userAccessLoggingSettingsArn =
        Prelude.Nothing
    }

-- | The ARN of the Kinesis stream.
userAccessLoggingSettingsSummary_kinesisStreamArn :: Lens.Lens' UserAccessLoggingSettingsSummary (Prelude.Maybe Prelude.Text)
userAccessLoggingSettingsSummary_kinesisStreamArn = Lens.lens (\UserAccessLoggingSettingsSummary' {kinesisStreamArn} -> kinesisStreamArn) (\s@UserAccessLoggingSettingsSummary' {} a -> s {kinesisStreamArn = a} :: UserAccessLoggingSettingsSummary)

-- | The ARN of the user access logging settings.
userAccessLoggingSettingsSummary_userAccessLoggingSettingsArn :: Lens.Lens' UserAccessLoggingSettingsSummary (Prelude.Maybe Prelude.Text)
userAccessLoggingSettingsSummary_userAccessLoggingSettingsArn = Lens.lens (\UserAccessLoggingSettingsSummary' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@UserAccessLoggingSettingsSummary' {} a -> s {userAccessLoggingSettingsArn = a} :: UserAccessLoggingSettingsSummary)

instance
  Core.FromJSON
    UserAccessLoggingSettingsSummary
  where
  parseJSON =
    Core.withObject
      "UserAccessLoggingSettingsSummary"
      ( \x ->
          UserAccessLoggingSettingsSummary'
            Prelude.<$> (x Core..:? "kinesisStreamArn")
            Prelude.<*> (x Core..:? "userAccessLoggingSettingsArn")
      )

instance
  Prelude.Hashable
    UserAccessLoggingSettingsSummary
  where
  hashWithSalt
    _salt
    UserAccessLoggingSettingsSummary' {..} =
      _salt `Prelude.hashWithSalt` kinesisStreamArn
        `Prelude.hashWithSalt` userAccessLoggingSettingsArn

instance
  Prelude.NFData
    UserAccessLoggingSettingsSummary
  where
  rnf UserAccessLoggingSettingsSummary' {..} =
    Prelude.rnf kinesisStreamArn
      `Prelude.seq` Prelude.rnf userAccessLoggingSettingsArn

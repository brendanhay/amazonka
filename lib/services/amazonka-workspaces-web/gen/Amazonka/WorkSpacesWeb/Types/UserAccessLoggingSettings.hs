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
-- Module      : Amazonka.WorkSpacesWeb.Types.UserAccessLoggingSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.UserAccessLoggingSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A user access logging settings resource that can be associated with a
-- web portal.
--
-- /See:/ 'newUserAccessLoggingSettings' smart constructor.
data UserAccessLoggingSettings = UserAccessLoggingSettings'
  { -- | A list of web portal ARNs that this user access logging settings is
    -- associated with.
    associatedPortalArns :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the Kinesis stream.
    kinesisStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user access logging settings.
    userAccessLoggingSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserAccessLoggingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedPortalArns', 'userAccessLoggingSettings_associatedPortalArns' - A list of web portal ARNs that this user access logging settings is
-- associated with.
--
-- 'kinesisStreamArn', 'userAccessLoggingSettings_kinesisStreamArn' - The ARN of the Kinesis stream.
--
-- 'userAccessLoggingSettingsArn', 'userAccessLoggingSettings_userAccessLoggingSettingsArn' - The ARN of the user access logging settings.
newUserAccessLoggingSettings ::
  -- | 'userAccessLoggingSettingsArn'
  Prelude.Text ->
  UserAccessLoggingSettings
newUserAccessLoggingSettings
  pUserAccessLoggingSettingsArn_ =
    UserAccessLoggingSettings'
      { associatedPortalArns =
          Prelude.Nothing,
        kinesisStreamArn = Prelude.Nothing,
        userAccessLoggingSettingsArn =
          pUserAccessLoggingSettingsArn_
      }

-- | A list of web portal ARNs that this user access logging settings is
-- associated with.
userAccessLoggingSettings_associatedPortalArns :: Lens.Lens' UserAccessLoggingSettings (Prelude.Maybe [Prelude.Text])
userAccessLoggingSettings_associatedPortalArns = Lens.lens (\UserAccessLoggingSettings' {associatedPortalArns} -> associatedPortalArns) (\s@UserAccessLoggingSettings' {} a -> s {associatedPortalArns = a} :: UserAccessLoggingSettings) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the Kinesis stream.
userAccessLoggingSettings_kinesisStreamArn :: Lens.Lens' UserAccessLoggingSettings (Prelude.Maybe Prelude.Text)
userAccessLoggingSettings_kinesisStreamArn = Lens.lens (\UserAccessLoggingSettings' {kinesisStreamArn} -> kinesisStreamArn) (\s@UserAccessLoggingSettings' {} a -> s {kinesisStreamArn = a} :: UserAccessLoggingSettings)

-- | The ARN of the user access logging settings.
userAccessLoggingSettings_userAccessLoggingSettingsArn :: Lens.Lens' UserAccessLoggingSettings Prelude.Text
userAccessLoggingSettings_userAccessLoggingSettingsArn = Lens.lens (\UserAccessLoggingSettings' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@UserAccessLoggingSettings' {} a -> s {userAccessLoggingSettingsArn = a} :: UserAccessLoggingSettings)

instance Data.FromJSON UserAccessLoggingSettings where
  parseJSON =
    Data.withObject
      "UserAccessLoggingSettings"
      ( \x ->
          UserAccessLoggingSettings'
            Prelude.<$> ( x Data..:? "associatedPortalArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "kinesisStreamArn")
            Prelude.<*> (x Data..: "userAccessLoggingSettingsArn")
      )

instance Prelude.Hashable UserAccessLoggingSettings where
  hashWithSalt _salt UserAccessLoggingSettings' {..} =
    _salt `Prelude.hashWithSalt` associatedPortalArns
      `Prelude.hashWithSalt` kinesisStreamArn
      `Prelude.hashWithSalt` userAccessLoggingSettingsArn

instance Prelude.NFData UserAccessLoggingSettings where
  rnf UserAccessLoggingSettings' {..} =
    Prelude.rnf associatedPortalArns
      `Prelude.seq` Prelude.rnf kinesisStreamArn
      `Prelude.seq` Prelude.rnf userAccessLoggingSettingsArn

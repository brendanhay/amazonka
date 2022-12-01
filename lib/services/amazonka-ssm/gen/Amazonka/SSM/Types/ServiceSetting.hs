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
-- Module      : Amazonka.SSM.Types.ServiceSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ServiceSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The service setting data structure.
--
-- @ServiceSetting@ is an account-level setting for an Amazon Web Services
-- service. This setting defines how a user interacts with or uses a
-- service or a feature of a service. For example, if an Amazon Web
-- Services service charges money to the account based on feature or
-- service usage, then the Amazon Web Services service team might create a
-- default setting of \"false\". This means the user can\'t use this
-- feature unless they change the setting to \"true\" and intentionally opt
-- in for a paid feature.
--
-- Services map a @SettingId@ object to a setting value. Amazon Web
-- Services services teams define the default value for a @SettingId@. You
-- can\'t create a new @SettingId@, but you can overwrite the default value
-- if you have the @ssm:UpdateServiceSetting@ permission for the setting.
-- Use the UpdateServiceSetting API operation to change the default
-- setting. Or, use the ResetServiceSetting to change the value back to the
-- original value defined by the Amazon Web Services service team.
--
-- /See:/ 'newServiceSetting' smart constructor.
data ServiceSetting = ServiceSetting'
  { -- | The ARN of the last modified user. This field is populated only if the
    -- setting value was overwritten.
    lastModifiedUser :: Prelude.Maybe Prelude.Text,
    -- | The last time the service setting was modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the service setting.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service setting.
    settingId :: Prelude.Maybe Prelude.Text,
    -- | The status of the service setting. The value can be Default, Customized
    -- or PendingUpdate.
    --
    -- -   Default: The current setting uses a default value provisioned by the
    --     Amazon Web Services service team.
    --
    -- -   Customized: The current setting use a custom value specified by the
    --     customer.
    --
    -- -   PendingUpdate: The current setting uses a default or custom value,
    --     but a setting change request is pending approval.
    status :: Prelude.Maybe Prelude.Text,
    -- | The value of the service setting.
    settingValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedUser', 'serviceSetting_lastModifiedUser' - The ARN of the last modified user. This field is populated only if the
-- setting value was overwritten.
--
-- 'lastModifiedDate', 'serviceSetting_lastModifiedDate' - The last time the service setting was modified.
--
-- 'arn', 'serviceSetting_arn' - The ARN of the service setting.
--
-- 'settingId', 'serviceSetting_settingId' - The ID of the service setting.
--
-- 'status', 'serviceSetting_status' - The status of the service setting. The value can be Default, Customized
-- or PendingUpdate.
--
-- -   Default: The current setting uses a default value provisioned by the
--     Amazon Web Services service team.
--
-- -   Customized: The current setting use a custom value specified by the
--     customer.
--
-- -   PendingUpdate: The current setting uses a default or custom value,
--     but a setting change request is pending approval.
--
-- 'settingValue', 'serviceSetting_settingValue' - The value of the service setting.
newServiceSetting ::
  ServiceSetting
newServiceSetting =
  ServiceSetting'
    { lastModifiedUser = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      settingId = Prelude.Nothing,
      status = Prelude.Nothing,
      settingValue = Prelude.Nothing
    }

-- | The ARN of the last modified user. This field is populated only if the
-- setting value was overwritten.
serviceSetting_lastModifiedUser :: Lens.Lens' ServiceSetting (Prelude.Maybe Prelude.Text)
serviceSetting_lastModifiedUser = Lens.lens (\ServiceSetting' {lastModifiedUser} -> lastModifiedUser) (\s@ServiceSetting' {} a -> s {lastModifiedUser = a} :: ServiceSetting)

-- | The last time the service setting was modified.
serviceSetting_lastModifiedDate :: Lens.Lens' ServiceSetting (Prelude.Maybe Prelude.UTCTime)
serviceSetting_lastModifiedDate = Lens.lens (\ServiceSetting' {lastModifiedDate} -> lastModifiedDate) (\s@ServiceSetting' {} a -> s {lastModifiedDate = a} :: ServiceSetting) Prelude.. Lens.mapping Core._Time

-- | The ARN of the service setting.
serviceSetting_arn :: Lens.Lens' ServiceSetting (Prelude.Maybe Prelude.Text)
serviceSetting_arn = Lens.lens (\ServiceSetting' {arn} -> arn) (\s@ServiceSetting' {} a -> s {arn = a} :: ServiceSetting)

-- | The ID of the service setting.
serviceSetting_settingId :: Lens.Lens' ServiceSetting (Prelude.Maybe Prelude.Text)
serviceSetting_settingId = Lens.lens (\ServiceSetting' {settingId} -> settingId) (\s@ServiceSetting' {} a -> s {settingId = a} :: ServiceSetting)

-- | The status of the service setting. The value can be Default, Customized
-- or PendingUpdate.
--
-- -   Default: The current setting uses a default value provisioned by the
--     Amazon Web Services service team.
--
-- -   Customized: The current setting use a custom value specified by the
--     customer.
--
-- -   PendingUpdate: The current setting uses a default or custom value,
--     but a setting change request is pending approval.
serviceSetting_status :: Lens.Lens' ServiceSetting (Prelude.Maybe Prelude.Text)
serviceSetting_status = Lens.lens (\ServiceSetting' {status} -> status) (\s@ServiceSetting' {} a -> s {status = a} :: ServiceSetting)

-- | The value of the service setting.
serviceSetting_settingValue :: Lens.Lens' ServiceSetting (Prelude.Maybe Prelude.Text)
serviceSetting_settingValue = Lens.lens (\ServiceSetting' {settingValue} -> settingValue) (\s@ServiceSetting' {} a -> s {settingValue = a} :: ServiceSetting)

instance Core.FromJSON ServiceSetting where
  parseJSON =
    Core.withObject
      "ServiceSetting"
      ( \x ->
          ServiceSetting'
            Prelude.<$> (x Core..:? "LastModifiedUser")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "SettingId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "SettingValue")
      )

instance Prelude.Hashable ServiceSetting where
  hashWithSalt _salt ServiceSetting' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedUser
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` settingId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` settingValue

instance Prelude.NFData ServiceSetting where
  rnf ServiceSetting' {..} =
    Prelude.rnf lastModifiedUser
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf settingId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf settingValue

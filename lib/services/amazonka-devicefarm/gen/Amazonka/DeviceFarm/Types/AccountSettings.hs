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
-- Module      : Amazonka.DeviceFarm.Types.AccountSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.AccountSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.DevicePlatform
import Amazonka.DeviceFarm.Types.TrialMinutes
import qualified Amazonka.Prelude as Prelude

-- | A container for account-level settings in AWS Device Farm.
--
-- /See:/ 'newAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { -- | The AWS account number specified in the @AccountSettings@ container.
    awsAccountNumber :: Prelude.Maybe Prelude.Text,
    -- | The default number of minutes (at the account level) a test run executes
    -- before it times out. The default value is 150 minutes.
    defaultJobTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of minutes a test run executes before it times out.
    maxJobTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of device slots that the AWS account can purchase.
    -- Each maximum is expressed as an @offering-id:number@ pair, where the
    -- @offering-id@ represents one of the IDs returned by the @ListOfferings@
    -- command.
    maxSlots :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int),
    -- | When set to @true@, for private devices, Device Farm does not sign your
    -- app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see
    -- <http://aws.amazon.com/device-farm/faqs/ Do you modify my app?> in the
    -- /AWS Device Farm FAQs/.
    skipAppResign :: Prelude.Maybe Prelude.Bool,
    -- | Information about an AWS account\'s usage of free trial device minutes.
    trialMinutes :: Prelude.Maybe TrialMinutes,
    -- | Returns the unmetered devices you have purchased or want to purchase.
    unmeteredDevices :: Prelude.Maybe (Prelude.HashMap DevicePlatform Prelude.Int),
    -- | Returns the unmetered remote access devices you have purchased or want
    -- to purchase.
    unmeteredRemoteAccessDevices :: Prelude.Maybe (Prelude.HashMap DevicePlatform Prelude.Int)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountNumber', 'accountSettings_awsAccountNumber' - The AWS account number specified in the @AccountSettings@ container.
--
-- 'defaultJobTimeoutMinutes', 'accountSettings_defaultJobTimeoutMinutes' - The default number of minutes (at the account level) a test run executes
-- before it times out. The default value is 150 minutes.
--
-- 'maxJobTimeoutMinutes', 'accountSettings_maxJobTimeoutMinutes' - The maximum number of minutes a test run executes before it times out.
--
-- 'maxSlots', 'accountSettings_maxSlots' - The maximum number of device slots that the AWS account can purchase.
-- Each maximum is expressed as an @offering-id:number@ pair, where the
-- @offering-id@ represents one of the IDs returned by the @ListOfferings@
-- command.
--
-- 'skipAppResign', 'accountSettings_skipAppResign' - When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <http://aws.amazon.com/device-farm/faqs/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
--
-- 'trialMinutes', 'accountSettings_trialMinutes' - Information about an AWS account\'s usage of free trial device minutes.
--
-- 'unmeteredDevices', 'accountSettings_unmeteredDevices' - Returns the unmetered devices you have purchased or want to purchase.
--
-- 'unmeteredRemoteAccessDevices', 'accountSettings_unmeteredRemoteAccessDevices' - Returns the unmetered remote access devices you have purchased or want
-- to purchase.
newAccountSettings ::
  AccountSettings
newAccountSettings =
  AccountSettings'
    { awsAccountNumber =
        Prelude.Nothing,
      defaultJobTimeoutMinutes = Prelude.Nothing,
      maxJobTimeoutMinutes = Prelude.Nothing,
      maxSlots = Prelude.Nothing,
      skipAppResign = Prelude.Nothing,
      trialMinutes = Prelude.Nothing,
      unmeteredDevices = Prelude.Nothing,
      unmeteredRemoteAccessDevices = Prelude.Nothing
    }

-- | The AWS account number specified in the @AccountSettings@ container.
accountSettings_awsAccountNumber :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_awsAccountNumber = Lens.lens (\AccountSettings' {awsAccountNumber} -> awsAccountNumber) (\s@AccountSettings' {} a -> s {awsAccountNumber = a} :: AccountSettings)

-- | The default number of minutes (at the account level) a test run executes
-- before it times out. The default value is 150 minutes.
accountSettings_defaultJobTimeoutMinutes :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Int)
accountSettings_defaultJobTimeoutMinutes = Lens.lens (\AccountSettings' {defaultJobTimeoutMinutes} -> defaultJobTimeoutMinutes) (\s@AccountSettings' {} a -> s {defaultJobTimeoutMinutes = a} :: AccountSettings)

-- | The maximum number of minutes a test run executes before it times out.
accountSettings_maxJobTimeoutMinutes :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Int)
accountSettings_maxJobTimeoutMinutes = Lens.lens (\AccountSettings' {maxJobTimeoutMinutes} -> maxJobTimeoutMinutes) (\s@AccountSettings' {} a -> s {maxJobTimeoutMinutes = a} :: AccountSettings)

-- | The maximum number of device slots that the AWS account can purchase.
-- Each maximum is expressed as an @offering-id:number@ pair, where the
-- @offering-id@ represents one of the IDs returned by the @ListOfferings@
-- command.
accountSettings_maxSlots :: Lens.Lens' AccountSettings (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
accountSettings_maxSlots = Lens.lens (\AccountSettings' {maxSlots} -> maxSlots) (\s@AccountSettings' {} a -> s {maxSlots = a} :: AccountSettings) Prelude.. Lens.mapping Lens.coerced

-- | When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <http://aws.amazon.com/device-farm/faqs/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
accountSettings_skipAppResign :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Bool)
accountSettings_skipAppResign = Lens.lens (\AccountSettings' {skipAppResign} -> skipAppResign) (\s@AccountSettings' {} a -> s {skipAppResign = a} :: AccountSettings)

-- | Information about an AWS account\'s usage of free trial device minutes.
accountSettings_trialMinutes :: Lens.Lens' AccountSettings (Prelude.Maybe TrialMinutes)
accountSettings_trialMinutes = Lens.lens (\AccountSettings' {trialMinutes} -> trialMinutes) (\s@AccountSettings' {} a -> s {trialMinutes = a} :: AccountSettings)

-- | Returns the unmetered devices you have purchased or want to purchase.
accountSettings_unmeteredDevices :: Lens.Lens' AccountSettings (Prelude.Maybe (Prelude.HashMap DevicePlatform Prelude.Int))
accountSettings_unmeteredDevices = Lens.lens (\AccountSettings' {unmeteredDevices} -> unmeteredDevices) (\s@AccountSettings' {} a -> s {unmeteredDevices = a} :: AccountSettings) Prelude.. Lens.mapping Lens.coerced

-- | Returns the unmetered remote access devices you have purchased or want
-- to purchase.
accountSettings_unmeteredRemoteAccessDevices :: Lens.Lens' AccountSettings (Prelude.Maybe (Prelude.HashMap DevicePlatform Prelude.Int))
accountSettings_unmeteredRemoteAccessDevices = Lens.lens (\AccountSettings' {unmeteredRemoteAccessDevices} -> unmeteredRemoteAccessDevices) (\s@AccountSettings' {} a -> s {unmeteredRemoteAccessDevices = a} :: AccountSettings) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AccountSettings where
  parseJSON =
    Data.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Prelude.<$> (x Data..:? "awsAccountNumber")
            Prelude.<*> (x Data..:? "defaultJobTimeoutMinutes")
            Prelude.<*> (x Data..:? "maxJobTimeoutMinutes")
            Prelude.<*> (x Data..:? "maxSlots" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "skipAppResign")
            Prelude.<*> (x Data..:? "trialMinutes")
            Prelude.<*> ( x Data..:? "unmeteredDevices"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "unmeteredRemoteAccessDevices"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AccountSettings where
  hashWithSalt _salt AccountSettings' {..} =
    _salt `Prelude.hashWithSalt` awsAccountNumber
      `Prelude.hashWithSalt` defaultJobTimeoutMinutes
      `Prelude.hashWithSalt` maxJobTimeoutMinutes
      `Prelude.hashWithSalt` maxSlots
      `Prelude.hashWithSalt` skipAppResign
      `Prelude.hashWithSalt` trialMinutes
      `Prelude.hashWithSalt` unmeteredDevices
      `Prelude.hashWithSalt` unmeteredRemoteAccessDevices

instance Prelude.NFData AccountSettings where
  rnf AccountSettings' {..} =
    Prelude.rnf awsAccountNumber
      `Prelude.seq` Prelude.rnf defaultJobTimeoutMinutes
      `Prelude.seq` Prelude.rnf maxJobTimeoutMinutes
      `Prelude.seq` Prelude.rnf maxSlots
      `Prelude.seq` Prelude.rnf skipAppResign
      `Prelude.seq` Prelude.rnf trialMinutes
      `Prelude.seq` Prelude.rnf unmeteredDevices
      `Prelude.seq` Prelude.rnf unmeteredRemoteAccessDevices

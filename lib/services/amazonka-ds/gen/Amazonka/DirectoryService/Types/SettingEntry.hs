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
-- Module      : Amazonka.DirectoryService.Types.SettingEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.SettingEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types.DirectoryConfigurationStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the specified configurable setting for a
-- directory.
--
-- /See:/ 'newSettingEntry' smart constructor.
data SettingEntry = SettingEntry'
  { -- | Details about the status of the request to update the directory setting.
    -- If the directory setting is deployed in more than one region, status is
    -- returned for the request in each region where the setting is deployed.
    requestDetailedStatus :: Prelude.Maybe (Prelude.HashMap Prelude.Text DirectoryConfigurationStatus),
    -- | The name of the directory setting. For example:
    --
    -- @TLS_1_0@
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of directory setting. For example, @Protocol@ or @Cipher@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The last status message for the directory status request.
    requestStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The value of the directory setting that is applied to the directory.
    appliedValue :: Prelude.Maybe Prelude.Text,
    -- | The value that was last requested for the directory setting.
    requestedValue :: Prelude.Maybe Prelude.Text,
    -- | The overall status of the request to update the directory setting
    -- request. If the directory setting is deployed in more than one region,
    -- and the request fails in any region, the overall status is @Failed@.
    requestStatus :: Prelude.Maybe DirectoryConfigurationStatus,
    -- | The valid range of values for the directory setting.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the directory setting was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time when the request to update a directory setting was
    -- last submitted.
    lastRequestedDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SettingEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestDetailedStatus', 'settingEntry_requestDetailedStatus' - Details about the status of the request to update the directory setting.
-- If the directory setting is deployed in more than one region, status is
-- returned for the request in each region where the setting is deployed.
--
-- 'name', 'settingEntry_name' - The name of the directory setting. For example:
--
-- @TLS_1_0@
--
-- 'type'', 'settingEntry_type' - The type of directory setting. For example, @Protocol@ or @Cipher@.
--
-- 'requestStatusMessage', 'settingEntry_requestStatusMessage' - The last status message for the directory status request.
--
-- 'appliedValue', 'settingEntry_appliedValue' - The value of the directory setting that is applied to the directory.
--
-- 'requestedValue', 'settingEntry_requestedValue' - The value that was last requested for the directory setting.
--
-- 'requestStatus', 'settingEntry_requestStatus' - The overall status of the request to update the directory setting
-- request. If the directory setting is deployed in more than one region,
-- and the request fails in any region, the overall status is @Failed@.
--
-- 'allowedValues', 'settingEntry_allowedValues' - The valid range of values for the directory setting.
--
-- 'lastUpdatedDateTime', 'settingEntry_lastUpdatedDateTime' - The date and time when the directory setting was last updated.
--
-- 'lastRequestedDateTime', 'settingEntry_lastRequestedDateTime' - The date and time when the request to update a directory setting was
-- last submitted.
newSettingEntry ::
  SettingEntry
newSettingEntry =
  SettingEntry'
    { requestDetailedStatus =
        Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      requestStatusMessage = Prelude.Nothing,
      appliedValue = Prelude.Nothing,
      requestedValue = Prelude.Nothing,
      requestStatus = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      lastRequestedDateTime = Prelude.Nothing
    }

-- | Details about the status of the request to update the directory setting.
-- If the directory setting is deployed in more than one region, status is
-- returned for the request in each region where the setting is deployed.
settingEntry_requestDetailedStatus :: Lens.Lens' SettingEntry (Prelude.Maybe (Prelude.HashMap Prelude.Text DirectoryConfigurationStatus))
settingEntry_requestDetailedStatus = Lens.lens (\SettingEntry' {requestDetailedStatus} -> requestDetailedStatus) (\s@SettingEntry' {} a -> s {requestDetailedStatus = a} :: SettingEntry) Prelude.. Lens.mapping Lens.coerced

-- | The name of the directory setting. For example:
--
-- @TLS_1_0@
settingEntry_name :: Lens.Lens' SettingEntry (Prelude.Maybe Prelude.Text)
settingEntry_name = Lens.lens (\SettingEntry' {name} -> name) (\s@SettingEntry' {} a -> s {name = a} :: SettingEntry)

-- | The type of directory setting. For example, @Protocol@ or @Cipher@.
settingEntry_type :: Lens.Lens' SettingEntry (Prelude.Maybe Prelude.Text)
settingEntry_type = Lens.lens (\SettingEntry' {type'} -> type') (\s@SettingEntry' {} a -> s {type' = a} :: SettingEntry)

-- | The last status message for the directory status request.
settingEntry_requestStatusMessage :: Lens.Lens' SettingEntry (Prelude.Maybe Prelude.Text)
settingEntry_requestStatusMessage = Lens.lens (\SettingEntry' {requestStatusMessage} -> requestStatusMessage) (\s@SettingEntry' {} a -> s {requestStatusMessage = a} :: SettingEntry)

-- | The value of the directory setting that is applied to the directory.
settingEntry_appliedValue :: Lens.Lens' SettingEntry (Prelude.Maybe Prelude.Text)
settingEntry_appliedValue = Lens.lens (\SettingEntry' {appliedValue} -> appliedValue) (\s@SettingEntry' {} a -> s {appliedValue = a} :: SettingEntry)

-- | The value that was last requested for the directory setting.
settingEntry_requestedValue :: Lens.Lens' SettingEntry (Prelude.Maybe Prelude.Text)
settingEntry_requestedValue = Lens.lens (\SettingEntry' {requestedValue} -> requestedValue) (\s@SettingEntry' {} a -> s {requestedValue = a} :: SettingEntry)

-- | The overall status of the request to update the directory setting
-- request. If the directory setting is deployed in more than one region,
-- and the request fails in any region, the overall status is @Failed@.
settingEntry_requestStatus :: Lens.Lens' SettingEntry (Prelude.Maybe DirectoryConfigurationStatus)
settingEntry_requestStatus = Lens.lens (\SettingEntry' {requestStatus} -> requestStatus) (\s@SettingEntry' {} a -> s {requestStatus = a} :: SettingEntry)

-- | The valid range of values for the directory setting.
settingEntry_allowedValues :: Lens.Lens' SettingEntry (Prelude.Maybe Prelude.Text)
settingEntry_allowedValues = Lens.lens (\SettingEntry' {allowedValues} -> allowedValues) (\s@SettingEntry' {} a -> s {allowedValues = a} :: SettingEntry)

-- | The date and time when the directory setting was last updated.
settingEntry_lastUpdatedDateTime :: Lens.Lens' SettingEntry (Prelude.Maybe Prelude.UTCTime)
settingEntry_lastUpdatedDateTime = Lens.lens (\SettingEntry' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SettingEntry' {} a -> s {lastUpdatedDateTime = a} :: SettingEntry) Prelude.. Lens.mapping Core._Time

-- | The date and time when the request to update a directory setting was
-- last submitted.
settingEntry_lastRequestedDateTime :: Lens.Lens' SettingEntry (Prelude.Maybe Prelude.UTCTime)
settingEntry_lastRequestedDateTime = Lens.lens (\SettingEntry' {lastRequestedDateTime} -> lastRequestedDateTime) (\s@SettingEntry' {} a -> s {lastRequestedDateTime = a} :: SettingEntry) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SettingEntry where
  parseJSON =
    Core.withObject
      "SettingEntry"
      ( \x ->
          SettingEntry'
            Prelude.<$> ( x Core..:? "RequestDetailedStatus"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "RequestStatusMessage")
            Prelude.<*> (x Core..:? "AppliedValue")
            Prelude.<*> (x Core..:? "RequestedValue")
            Prelude.<*> (x Core..:? "RequestStatus")
            Prelude.<*> (x Core..:? "AllowedValues")
            Prelude.<*> (x Core..:? "LastUpdatedDateTime")
            Prelude.<*> (x Core..:? "LastRequestedDateTime")
      )

instance Prelude.Hashable SettingEntry where
  hashWithSalt _salt SettingEntry' {..} =
    _salt `Prelude.hashWithSalt` requestDetailedStatus
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` requestStatusMessage
      `Prelude.hashWithSalt` appliedValue
      `Prelude.hashWithSalt` requestedValue
      `Prelude.hashWithSalt` requestStatus
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` lastRequestedDateTime

instance Prelude.NFData SettingEntry where
  rnf SettingEntry' {..} =
    Prelude.rnf requestDetailedStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf requestStatusMessage
      `Prelude.seq` Prelude.rnf appliedValue
      `Prelude.seq` Prelude.rnf requestedValue
      `Prelude.seq` Prelude.rnf requestStatus
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf lastRequestedDateTime

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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationRestoreConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationRestoreConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ApplicationRestoreType
import qualified Amazonka.Prelude as Prelude

-- | Specifies the method and snapshot to use when restarting an application
-- using previously saved application state.
--
-- /See:/ 'newApplicationRestoreConfiguration' smart constructor.
data ApplicationRestoreConfiguration = ApplicationRestoreConfiguration'
  { -- | The identifier of an existing snapshot of application state to use to
    -- restart an application. The application uses this value if
    -- @RESTORE_FROM_CUSTOM_SNAPSHOT@ is specified for the
    -- @ApplicationRestoreType@.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | Specifies how the application should be restored.
    applicationRestoreType :: ApplicationRestoreType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationRestoreConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotName', 'applicationRestoreConfiguration_snapshotName' - The identifier of an existing snapshot of application state to use to
-- restart an application. The application uses this value if
-- @RESTORE_FROM_CUSTOM_SNAPSHOT@ is specified for the
-- @ApplicationRestoreType@.
--
-- 'applicationRestoreType', 'applicationRestoreConfiguration_applicationRestoreType' - Specifies how the application should be restored.
newApplicationRestoreConfiguration ::
  -- | 'applicationRestoreType'
  ApplicationRestoreType ->
  ApplicationRestoreConfiguration
newApplicationRestoreConfiguration
  pApplicationRestoreType_ =
    ApplicationRestoreConfiguration'
      { snapshotName =
          Prelude.Nothing,
        applicationRestoreType =
          pApplicationRestoreType_
      }

-- | The identifier of an existing snapshot of application state to use to
-- restart an application. The application uses this value if
-- @RESTORE_FROM_CUSTOM_SNAPSHOT@ is specified for the
-- @ApplicationRestoreType@.
applicationRestoreConfiguration_snapshotName :: Lens.Lens' ApplicationRestoreConfiguration (Prelude.Maybe Prelude.Text)
applicationRestoreConfiguration_snapshotName = Lens.lens (\ApplicationRestoreConfiguration' {snapshotName} -> snapshotName) (\s@ApplicationRestoreConfiguration' {} a -> s {snapshotName = a} :: ApplicationRestoreConfiguration)

-- | Specifies how the application should be restored.
applicationRestoreConfiguration_applicationRestoreType :: Lens.Lens' ApplicationRestoreConfiguration ApplicationRestoreType
applicationRestoreConfiguration_applicationRestoreType = Lens.lens (\ApplicationRestoreConfiguration' {applicationRestoreType} -> applicationRestoreType) (\s@ApplicationRestoreConfiguration' {} a -> s {applicationRestoreType = a} :: ApplicationRestoreConfiguration)

instance
  Data.FromJSON
    ApplicationRestoreConfiguration
  where
  parseJSON =
    Data.withObject
      "ApplicationRestoreConfiguration"
      ( \x ->
          ApplicationRestoreConfiguration'
            Prelude.<$> (x Data..:? "SnapshotName")
            Prelude.<*> (x Data..: "ApplicationRestoreType")
      )

instance
  Prelude.Hashable
    ApplicationRestoreConfiguration
  where
  hashWithSalt
    _salt
    ApplicationRestoreConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` snapshotName
        `Prelude.hashWithSalt` applicationRestoreType

instance
  Prelude.NFData
    ApplicationRestoreConfiguration
  where
  rnf ApplicationRestoreConfiguration' {..} =
    Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf applicationRestoreType

instance Data.ToJSON ApplicationRestoreConfiguration where
  toJSON ApplicationRestoreConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SnapshotName" Data..=) Prelude.<$> snapshotName,
            Prelude.Just
              ( "ApplicationRestoreType"
                  Data..= applicationRestoreType
              )
          ]
      )

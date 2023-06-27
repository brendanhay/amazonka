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
-- Module      : Amazonka.AuditManager.Types.DefaultExportDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.DefaultExportDestination where

import Amazonka.AuditManager.Types.ExportDestinationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The default s3 bucket where Audit Manager saves the files that you
-- export from evidence finder.
--
-- /See:/ 'newDefaultExportDestination' smart constructor.
data DefaultExportDestination = DefaultExportDestination'
  { -- | The destination bucket where Audit Manager stores exported files.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The destination type, such as Amazon S3.
    destinationType :: Prelude.Maybe ExportDestinationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultExportDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'defaultExportDestination_destination' - The destination bucket where Audit Manager stores exported files.
--
-- 'destinationType', 'defaultExportDestination_destinationType' - The destination type, such as Amazon S3.
newDefaultExportDestination ::
  DefaultExportDestination
newDefaultExportDestination =
  DefaultExportDestination'
    { destination =
        Prelude.Nothing,
      destinationType = Prelude.Nothing
    }

-- | The destination bucket where Audit Manager stores exported files.
defaultExportDestination_destination :: Lens.Lens' DefaultExportDestination (Prelude.Maybe Prelude.Text)
defaultExportDestination_destination = Lens.lens (\DefaultExportDestination' {destination} -> destination) (\s@DefaultExportDestination' {} a -> s {destination = a} :: DefaultExportDestination)

-- | The destination type, such as Amazon S3.
defaultExportDestination_destinationType :: Lens.Lens' DefaultExportDestination (Prelude.Maybe ExportDestinationType)
defaultExportDestination_destinationType = Lens.lens (\DefaultExportDestination' {destinationType} -> destinationType) (\s@DefaultExportDestination' {} a -> s {destinationType = a} :: DefaultExportDestination)

instance Data.FromJSON DefaultExportDestination where
  parseJSON =
    Data.withObject
      "DefaultExportDestination"
      ( \x ->
          DefaultExportDestination'
            Prelude.<$> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "destinationType")
      )

instance Prelude.Hashable DefaultExportDestination where
  hashWithSalt _salt DefaultExportDestination' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationType

instance Prelude.NFData DefaultExportDestination where
  rnf DefaultExportDestination' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationType

instance Data.ToJSON DefaultExportDestination where
  toJSON DefaultExportDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destination" Data..=) Prelude.<$> destination,
            ("destinationType" Data..=)
              Prelude.<$> destinationType
          ]
      )

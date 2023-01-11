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
-- Module      : Amazonka.SecurityHub.Types.Threat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Threat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.FilePaths

-- | Provides information about the threat detected in a security finding and
-- the file paths that were affected by the threat.
--
-- /See:/ 'newThreat' smart constructor.
data Threat = Threat'
  { -- | Provides information about the file paths that were affected by the
    -- threat.
    filePaths :: Prelude.Maybe [FilePaths],
    -- | This total number of items in which the threat has been detected.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the threat.
    name :: Prelude.Maybe Prelude.Text,
    -- | The severity of the threat.
    severity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Threat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePaths', 'threat_filePaths' - Provides information about the file paths that were affected by the
-- threat.
--
-- 'itemCount', 'threat_itemCount' - This total number of items in which the threat has been detected.
--
-- 'name', 'threat_name' - The name of the threat.
--
-- 'severity', 'threat_severity' - The severity of the threat.
newThreat ::
  Threat
newThreat =
  Threat'
    { filePaths = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      name = Prelude.Nothing,
      severity = Prelude.Nothing
    }

-- | Provides information about the file paths that were affected by the
-- threat.
threat_filePaths :: Lens.Lens' Threat (Prelude.Maybe [FilePaths])
threat_filePaths = Lens.lens (\Threat' {filePaths} -> filePaths) (\s@Threat' {} a -> s {filePaths = a} :: Threat) Prelude.. Lens.mapping Lens.coerced

-- | This total number of items in which the threat has been detected.
threat_itemCount :: Lens.Lens' Threat (Prelude.Maybe Prelude.Int)
threat_itemCount = Lens.lens (\Threat' {itemCount} -> itemCount) (\s@Threat' {} a -> s {itemCount = a} :: Threat)

-- | The name of the threat.
threat_name :: Lens.Lens' Threat (Prelude.Maybe Prelude.Text)
threat_name = Lens.lens (\Threat' {name} -> name) (\s@Threat' {} a -> s {name = a} :: Threat)

-- | The severity of the threat.
threat_severity :: Lens.Lens' Threat (Prelude.Maybe Prelude.Text)
threat_severity = Lens.lens (\Threat' {severity} -> severity) (\s@Threat' {} a -> s {severity = a} :: Threat)

instance Data.FromJSON Threat where
  parseJSON =
    Data.withObject
      "Threat"
      ( \x ->
          Threat'
            Prelude.<$> (x Data..:? "FilePaths" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ItemCount")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Severity")
      )

instance Prelude.Hashable Threat where
  hashWithSalt _salt Threat' {..} =
    _salt `Prelude.hashWithSalt` filePaths
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` severity

instance Prelude.NFData Threat where
  rnf Threat' {..} =
    Prelude.rnf filePaths
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf severity

instance Data.ToJSON Threat where
  toJSON Threat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilePaths" Data..=) Prelude.<$> filePaths,
            ("ItemCount" Data..=) Prelude.<$> itemCount,
            ("Name" Data..=) Prelude.<$> name,
            ("Severity" Data..=) Prelude.<$> severity
          ]
      )

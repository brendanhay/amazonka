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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | The severity of the threat.
    severity :: Prelude.Maybe Prelude.Text,
    -- | The name of the threat.
    name :: Prelude.Maybe Prelude.Text,
    -- | This total number of items in which the threat has been detected.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | Provides information about the file paths that were affected by the
    -- threat.
    filePaths :: Prelude.Maybe [FilePaths]
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
-- 'severity', 'threat_severity' - The severity of the threat.
--
-- 'name', 'threat_name' - The name of the threat.
--
-- 'itemCount', 'threat_itemCount' - This total number of items in which the threat has been detected.
--
-- 'filePaths', 'threat_filePaths' - Provides information about the file paths that were affected by the
-- threat.
newThreat ::
  Threat
newThreat =
  Threat'
    { severity = Prelude.Nothing,
      name = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      filePaths = Prelude.Nothing
    }

-- | The severity of the threat.
threat_severity :: Lens.Lens' Threat (Prelude.Maybe Prelude.Text)
threat_severity = Lens.lens (\Threat' {severity} -> severity) (\s@Threat' {} a -> s {severity = a} :: Threat)

-- | The name of the threat.
threat_name :: Lens.Lens' Threat (Prelude.Maybe Prelude.Text)
threat_name = Lens.lens (\Threat' {name} -> name) (\s@Threat' {} a -> s {name = a} :: Threat)

-- | This total number of items in which the threat has been detected.
threat_itemCount :: Lens.Lens' Threat (Prelude.Maybe Prelude.Int)
threat_itemCount = Lens.lens (\Threat' {itemCount} -> itemCount) (\s@Threat' {} a -> s {itemCount = a} :: Threat)

-- | Provides information about the file paths that were affected by the
-- threat.
threat_filePaths :: Lens.Lens' Threat (Prelude.Maybe [FilePaths])
threat_filePaths = Lens.lens (\Threat' {filePaths} -> filePaths) (\s@Threat' {} a -> s {filePaths = a} :: Threat) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Threat where
  parseJSON =
    Data.withObject
      "Threat"
      ( \x ->
          Threat'
            Prelude.<$> (x Data..:? "Severity")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ItemCount")
            Prelude.<*> (x Data..:? "FilePaths" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Threat where
  hashWithSalt _salt Threat' {..} =
    _salt `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` filePaths

instance Prelude.NFData Threat where
  rnf Threat' {..} =
    Prelude.rnf severity
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf filePaths

instance Data.ToJSON Threat where
  toJSON Threat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Severity" Data..=) Prelude.<$> severity,
            ("Name" Data..=) Prelude.<$> name,
            ("ItemCount" Data..=) Prelude.<$> itemCount,
            ("FilePaths" Data..=) Prelude.<$> filePaths
          ]
      )

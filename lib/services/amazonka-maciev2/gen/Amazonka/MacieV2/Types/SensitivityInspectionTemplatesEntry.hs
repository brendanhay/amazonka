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
-- Module      : Amazonka.MacieV2.Types.SensitivityInspectionTemplatesEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SensitivityInspectionTemplatesEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the sensitivity inspection template for an
-- Amazon Macie account. Macie uses the template\'s settings when it
-- performs automated sensitive data discovery for the account.
--
-- /See:/ 'newSensitivityInspectionTemplatesEntry' smart constructor.
data SensitivityInspectionTemplatesEntry = SensitivityInspectionTemplatesEntry'
  { -- | The unique identifier for the sensitivity inspection template for the
    -- account.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the sensitivity inspection template for the account.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SensitivityInspectionTemplatesEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'sensitivityInspectionTemplatesEntry_id' - The unique identifier for the sensitivity inspection template for the
-- account.
--
-- 'name', 'sensitivityInspectionTemplatesEntry_name' - The name of the sensitivity inspection template for the account.
newSensitivityInspectionTemplatesEntry ::
  SensitivityInspectionTemplatesEntry
newSensitivityInspectionTemplatesEntry =
  SensitivityInspectionTemplatesEntry'
    { id =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The unique identifier for the sensitivity inspection template for the
-- account.
sensitivityInspectionTemplatesEntry_id :: Lens.Lens' SensitivityInspectionTemplatesEntry (Prelude.Maybe Prelude.Text)
sensitivityInspectionTemplatesEntry_id = Lens.lens (\SensitivityInspectionTemplatesEntry' {id} -> id) (\s@SensitivityInspectionTemplatesEntry' {} a -> s {id = a} :: SensitivityInspectionTemplatesEntry)

-- | The name of the sensitivity inspection template for the account.
sensitivityInspectionTemplatesEntry_name :: Lens.Lens' SensitivityInspectionTemplatesEntry (Prelude.Maybe Prelude.Text)
sensitivityInspectionTemplatesEntry_name = Lens.lens (\SensitivityInspectionTemplatesEntry' {name} -> name) (\s@SensitivityInspectionTemplatesEntry' {} a -> s {name = a} :: SensitivityInspectionTemplatesEntry)

instance
  Data.FromJSON
    SensitivityInspectionTemplatesEntry
  where
  parseJSON =
    Data.withObject
      "SensitivityInspectionTemplatesEntry"
      ( \x ->
          SensitivityInspectionTemplatesEntry'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    SensitivityInspectionTemplatesEntry
  where
  hashWithSalt
    _salt
    SensitivityInspectionTemplatesEntry' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    SensitivityInspectionTemplatesEntry
  where
  rnf SensitivityInspectionTemplatesEntry' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name

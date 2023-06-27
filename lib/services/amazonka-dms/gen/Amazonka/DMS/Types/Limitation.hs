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
-- Module      : Amazonka.DMS.Types.Limitation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.Limitation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the limitations of target Amazon Web Services
-- engines.
--
-- Your source database might include features that the target Amazon Web
-- Services engine doesn\'t support. Fleet Advisor lists these features as
-- limitations. You should consider these limitations during database
-- migration. For each limitation, Fleet Advisor recommends an action that
-- you can take to address or avoid this limitation.
--
-- /See:/ 'newLimitation' smart constructor.
data Limitation = Limitation'
  { -- | The identifier of the source database.
    databaseId :: Prelude.Maybe Prelude.Text,
    -- | A description of the limitation. Provides additional information about
    -- the limitation, and includes recommended actions that you can take to
    -- address or avoid this limitation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the target engine that Fleet Advisor should use in the
    -- target engine recommendation. Valid values include
    -- @\"rds-aurora-mysql\"@, @\"rds-aurora-postgresql\"@, @\"rds-mysql\"@,
    -- @\"rds-oracle\"@, @\"rds-sql-server\"@, and @\"rds-postgresql\"@.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | The impact of the limitation. You can use this parameter to prioritize
    -- limitations that you want to address. Valid values include
    -- @\"Blocker\"@, @\"High\"@, @\"Medium\"@, and @\"Low\"@.
    impact :: Prelude.Maybe Prelude.Text,
    -- | The name of the limitation. Describes unsupported database features,
    -- migration action items, and other limitations.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the limitation, such as action required, upgrade required,
    -- and limited feature.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Limitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseId', 'limitation_databaseId' - The identifier of the source database.
--
-- 'description', 'limitation_description' - A description of the limitation. Provides additional information about
-- the limitation, and includes recommended actions that you can take to
-- address or avoid this limitation.
--
-- 'engineName', 'limitation_engineName' - The name of the target engine that Fleet Advisor should use in the
-- target engine recommendation. Valid values include
-- @\"rds-aurora-mysql\"@, @\"rds-aurora-postgresql\"@, @\"rds-mysql\"@,
-- @\"rds-oracle\"@, @\"rds-sql-server\"@, and @\"rds-postgresql\"@.
--
-- 'impact', 'limitation_impact' - The impact of the limitation. You can use this parameter to prioritize
-- limitations that you want to address. Valid values include
-- @\"Blocker\"@, @\"High\"@, @\"Medium\"@, and @\"Low\"@.
--
-- 'name', 'limitation_name' - The name of the limitation. Describes unsupported database features,
-- migration action items, and other limitations.
--
-- 'type'', 'limitation_type' - The type of the limitation, such as action required, upgrade required,
-- and limited feature.
newLimitation ::
  Limitation
newLimitation =
  Limitation'
    { databaseId = Prelude.Nothing,
      description = Prelude.Nothing,
      engineName = Prelude.Nothing,
      impact = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The identifier of the source database.
limitation_databaseId :: Lens.Lens' Limitation (Prelude.Maybe Prelude.Text)
limitation_databaseId = Lens.lens (\Limitation' {databaseId} -> databaseId) (\s@Limitation' {} a -> s {databaseId = a} :: Limitation)

-- | A description of the limitation. Provides additional information about
-- the limitation, and includes recommended actions that you can take to
-- address or avoid this limitation.
limitation_description :: Lens.Lens' Limitation (Prelude.Maybe Prelude.Text)
limitation_description = Lens.lens (\Limitation' {description} -> description) (\s@Limitation' {} a -> s {description = a} :: Limitation)

-- | The name of the target engine that Fleet Advisor should use in the
-- target engine recommendation. Valid values include
-- @\"rds-aurora-mysql\"@, @\"rds-aurora-postgresql\"@, @\"rds-mysql\"@,
-- @\"rds-oracle\"@, @\"rds-sql-server\"@, and @\"rds-postgresql\"@.
limitation_engineName :: Lens.Lens' Limitation (Prelude.Maybe Prelude.Text)
limitation_engineName = Lens.lens (\Limitation' {engineName} -> engineName) (\s@Limitation' {} a -> s {engineName = a} :: Limitation)

-- | The impact of the limitation. You can use this parameter to prioritize
-- limitations that you want to address. Valid values include
-- @\"Blocker\"@, @\"High\"@, @\"Medium\"@, and @\"Low\"@.
limitation_impact :: Lens.Lens' Limitation (Prelude.Maybe Prelude.Text)
limitation_impact = Lens.lens (\Limitation' {impact} -> impact) (\s@Limitation' {} a -> s {impact = a} :: Limitation)

-- | The name of the limitation. Describes unsupported database features,
-- migration action items, and other limitations.
limitation_name :: Lens.Lens' Limitation (Prelude.Maybe Prelude.Text)
limitation_name = Lens.lens (\Limitation' {name} -> name) (\s@Limitation' {} a -> s {name = a} :: Limitation)

-- | The type of the limitation, such as action required, upgrade required,
-- and limited feature.
limitation_type :: Lens.Lens' Limitation (Prelude.Maybe Prelude.Text)
limitation_type = Lens.lens (\Limitation' {type'} -> type') (\s@Limitation' {} a -> s {type' = a} :: Limitation)

instance Data.FromJSON Limitation where
  parseJSON =
    Data.withObject
      "Limitation"
      ( \x ->
          Limitation'
            Prelude.<$> (x Data..:? "DatabaseId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EngineName")
            Prelude.<*> (x Data..:? "Impact")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Limitation where
  hashWithSalt _salt Limitation' {..} =
    _salt
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` engineName
      `Prelude.hashWithSalt` impact
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Limitation where
  rnf Limitation' {..} =
    Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf engineName
      `Prelude.seq` Prelude.rnf impact
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

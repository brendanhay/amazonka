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
-- Module      : Amazonka.AppFlow.Types.PrefixConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PrefixConfig where

import Amazonka.AppFlow.Types.PathPrefix
import Amazonka.AppFlow.Types.PrefixFormat
import Amazonka.AppFlow.Types.PrefixType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies elements that Amazon AppFlow includes in the file and folder
-- names in the flow destination.
--
-- /See:/ 'newPrefixConfig' smart constructor.
data PrefixConfig = PrefixConfig'
  { -- | Specifies whether the destination file path includes either or both of
    -- the following elements:
    --
    -- [EXECUTION_ID]
    --     The ID that Amazon AppFlow assigns to the flow run.
    --
    -- [SCHEMA_VERSION]
    --     The version number of your data schema. Amazon AppFlow assigns this
    --     version number. The version number increases by one when you change
    --     any of the following settings in your flow configuration:
    --
    --     -   Source-to-destination field mappings
    --
    --     -   Field data types
    --
    --     -   Partition keys
    pathPrefixHierarchy :: Prelude.Maybe [PathPrefix],
    -- | Determines the level of granularity for the date and time that\'s
    -- included in the prefix.
    prefixFormat :: Prelude.Maybe PrefixFormat,
    -- | Determines the format of the prefix, and whether it applies to the file
    -- name, file path, or both.
    prefixType :: Prelude.Maybe PrefixType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrefixConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathPrefixHierarchy', 'prefixConfig_pathPrefixHierarchy' - Specifies whether the destination file path includes either or both of
-- the following elements:
--
-- [EXECUTION_ID]
--     The ID that Amazon AppFlow assigns to the flow run.
--
-- [SCHEMA_VERSION]
--     The version number of your data schema. Amazon AppFlow assigns this
--     version number. The version number increases by one when you change
--     any of the following settings in your flow configuration:
--
--     -   Source-to-destination field mappings
--
--     -   Field data types
--
--     -   Partition keys
--
-- 'prefixFormat', 'prefixConfig_prefixFormat' - Determines the level of granularity for the date and time that\'s
-- included in the prefix.
--
-- 'prefixType', 'prefixConfig_prefixType' - Determines the format of the prefix, and whether it applies to the file
-- name, file path, or both.
newPrefixConfig ::
  PrefixConfig
newPrefixConfig =
  PrefixConfig'
    { pathPrefixHierarchy =
        Prelude.Nothing,
      prefixFormat = Prelude.Nothing,
      prefixType = Prelude.Nothing
    }

-- | Specifies whether the destination file path includes either or both of
-- the following elements:
--
-- [EXECUTION_ID]
--     The ID that Amazon AppFlow assigns to the flow run.
--
-- [SCHEMA_VERSION]
--     The version number of your data schema. Amazon AppFlow assigns this
--     version number. The version number increases by one when you change
--     any of the following settings in your flow configuration:
--
--     -   Source-to-destination field mappings
--
--     -   Field data types
--
--     -   Partition keys
prefixConfig_pathPrefixHierarchy :: Lens.Lens' PrefixConfig (Prelude.Maybe [PathPrefix])
prefixConfig_pathPrefixHierarchy = Lens.lens (\PrefixConfig' {pathPrefixHierarchy} -> pathPrefixHierarchy) (\s@PrefixConfig' {} a -> s {pathPrefixHierarchy = a} :: PrefixConfig) Prelude.. Lens.mapping Lens.coerced

-- | Determines the level of granularity for the date and time that\'s
-- included in the prefix.
prefixConfig_prefixFormat :: Lens.Lens' PrefixConfig (Prelude.Maybe PrefixFormat)
prefixConfig_prefixFormat = Lens.lens (\PrefixConfig' {prefixFormat} -> prefixFormat) (\s@PrefixConfig' {} a -> s {prefixFormat = a} :: PrefixConfig)

-- | Determines the format of the prefix, and whether it applies to the file
-- name, file path, or both.
prefixConfig_prefixType :: Lens.Lens' PrefixConfig (Prelude.Maybe PrefixType)
prefixConfig_prefixType = Lens.lens (\PrefixConfig' {prefixType} -> prefixType) (\s@PrefixConfig' {} a -> s {prefixType = a} :: PrefixConfig)

instance Data.FromJSON PrefixConfig where
  parseJSON =
    Data.withObject
      "PrefixConfig"
      ( \x ->
          PrefixConfig'
            Prelude.<$> ( x
                            Data..:? "pathPrefixHierarchy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "prefixFormat")
            Prelude.<*> (x Data..:? "prefixType")
      )

instance Prelude.Hashable PrefixConfig where
  hashWithSalt _salt PrefixConfig' {..} =
    _salt
      `Prelude.hashWithSalt` pathPrefixHierarchy
      `Prelude.hashWithSalt` prefixFormat
      `Prelude.hashWithSalt` prefixType

instance Prelude.NFData PrefixConfig where
  rnf PrefixConfig' {..} =
    Prelude.rnf pathPrefixHierarchy
      `Prelude.seq` Prelude.rnf prefixFormat
      `Prelude.seq` Prelude.rnf prefixType

instance Data.ToJSON PrefixConfig where
  toJSON PrefixConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("pathPrefixHierarchy" Data..=)
              Prelude.<$> pathPrefixHierarchy,
            ("prefixFormat" Data..=) Prelude.<$> prefixFormat,
            ("prefixType" Data..=) Prelude.<$> prefixType
          ]
      )

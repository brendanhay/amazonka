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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PrefixConfig where

import Amazonka.AppFlow.Types.PrefixFormat
import Amazonka.AppFlow.Types.PrefixType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Determines the prefix that Amazon AppFlow applies to the destination
-- folder name. You can name your destination folders according to the flow
-- frequency and date.
--
-- /See:/ 'newPrefixConfig' smart constructor.
data PrefixConfig = PrefixConfig'
  { -- | Determines the format of the prefix, and whether it applies to the file
    -- name, file path, or both.
    prefixType :: Prelude.Maybe PrefixType,
    -- | Determines the level of granularity that\'s included in the prefix.
    prefixFormat :: Prelude.Maybe PrefixFormat
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
-- 'prefixType', 'prefixConfig_prefixType' - Determines the format of the prefix, and whether it applies to the file
-- name, file path, or both.
--
-- 'prefixFormat', 'prefixConfig_prefixFormat' - Determines the level of granularity that\'s included in the prefix.
newPrefixConfig ::
  PrefixConfig
newPrefixConfig =
  PrefixConfig'
    { prefixType = Prelude.Nothing,
      prefixFormat = Prelude.Nothing
    }

-- | Determines the format of the prefix, and whether it applies to the file
-- name, file path, or both.
prefixConfig_prefixType :: Lens.Lens' PrefixConfig (Prelude.Maybe PrefixType)
prefixConfig_prefixType = Lens.lens (\PrefixConfig' {prefixType} -> prefixType) (\s@PrefixConfig' {} a -> s {prefixType = a} :: PrefixConfig)

-- | Determines the level of granularity that\'s included in the prefix.
prefixConfig_prefixFormat :: Lens.Lens' PrefixConfig (Prelude.Maybe PrefixFormat)
prefixConfig_prefixFormat = Lens.lens (\PrefixConfig' {prefixFormat} -> prefixFormat) (\s@PrefixConfig' {} a -> s {prefixFormat = a} :: PrefixConfig)

instance Core.FromJSON PrefixConfig where
  parseJSON =
    Core.withObject
      "PrefixConfig"
      ( \x ->
          PrefixConfig'
            Prelude.<$> (x Core..:? "prefixType")
            Prelude.<*> (x Core..:? "prefixFormat")
      )

instance Prelude.Hashable PrefixConfig where
  hashWithSalt _salt PrefixConfig' {..} =
    _salt `Prelude.hashWithSalt` prefixType
      `Prelude.hashWithSalt` prefixFormat

instance Prelude.NFData PrefixConfig where
  rnf PrefixConfig' {..} =
    Prelude.rnf prefixType
      `Prelude.seq` Prelude.rnf prefixFormat

instance Core.ToJSON PrefixConfig where
  toJSON PrefixConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("prefixType" Core..=) Prelude.<$> prefixType,
            ("prefixFormat" Core..=) Prelude.<$> prefixFormat
          ]
      )

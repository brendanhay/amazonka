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
-- Module      : Amazonka.CustomerProfiles.Types.IncrementalPullConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.IncrementalPullConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration used when importing incremental records from
-- the source.
--
-- /See:/ 'newIncrementalPullConfig' smart constructor.
data IncrementalPullConfig = IncrementalPullConfig'
  { -- | A field that specifies the date time or timestamp field as the criteria
    -- to use when importing incremental records from the source.
    datetimeTypeFieldName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncrementalPullConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datetimeTypeFieldName', 'incrementalPullConfig_datetimeTypeFieldName' - A field that specifies the date time or timestamp field as the criteria
-- to use when importing incremental records from the source.
newIncrementalPullConfig ::
  IncrementalPullConfig
newIncrementalPullConfig =
  IncrementalPullConfig'
    { datetimeTypeFieldName =
        Prelude.Nothing
    }

-- | A field that specifies the date time or timestamp field as the criteria
-- to use when importing incremental records from the source.
incrementalPullConfig_datetimeTypeFieldName :: Lens.Lens' IncrementalPullConfig (Prelude.Maybe Prelude.Text)
incrementalPullConfig_datetimeTypeFieldName = Lens.lens (\IncrementalPullConfig' {datetimeTypeFieldName} -> datetimeTypeFieldName) (\s@IncrementalPullConfig' {} a -> s {datetimeTypeFieldName = a} :: IncrementalPullConfig)

instance Prelude.Hashable IncrementalPullConfig where
  hashWithSalt _salt IncrementalPullConfig' {..} =
    _salt `Prelude.hashWithSalt` datetimeTypeFieldName

instance Prelude.NFData IncrementalPullConfig where
  rnf IncrementalPullConfig' {..} =
    Prelude.rnf datetimeTypeFieldName

instance Data.ToJSON IncrementalPullConfig where
  toJSON IncrementalPullConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DatetimeTypeFieldName" Data..=)
              Prelude.<$> datetimeTypeFieldName
          ]
      )

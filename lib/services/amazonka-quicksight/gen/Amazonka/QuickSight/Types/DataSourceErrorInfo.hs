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
-- Module      : Amazonka.QuickSight.Types.DataSourceErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSourceErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSourceErrorInfoType

-- | Error information for the data source creation or update.
--
-- /See:/ 'newDataSourceErrorInfo' smart constructor.
data DataSourceErrorInfo = DataSourceErrorInfo'
  { -- | Error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | Error type.
    type' :: Prelude.Maybe DataSourceErrorInfoType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'dataSourceErrorInfo_message' - Error message.
--
-- 'type'', 'dataSourceErrorInfo_type' - Error type.
newDataSourceErrorInfo ::
  DataSourceErrorInfo
newDataSourceErrorInfo =
  DataSourceErrorInfo'
    { message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Error message.
dataSourceErrorInfo_message :: Lens.Lens' DataSourceErrorInfo (Prelude.Maybe Prelude.Text)
dataSourceErrorInfo_message = Lens.lens (\DataSourceErrorInfo' {message} -> message) (\s@DataSourceErrorInfo' {} a -> s {message = a} :: DataSourceErrorInfo)

-- | Error type.
dataSourceErrorInfo_type :: Lens.Lens' DataSourceErrorInfo (Prelude.Maybe DataSourceErrorInfoType)
dataSourceErrorInfo_type = Lens.lens (\DataSourceErrorInfo' {type'} -> type') (\s@DataSourceErrorInfo' {} a -> s {type' = a} :: DataSourceErrorInfo)

instance Data.FromJSON DataSourceErrorInfo where
  parseJSON =
    Data.withObject
      "DataSourceErrorInfo"
      ( \x ->
          DataSourceErrorInfo'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable DataSourceErrorInfo where
  hashWithSalt _salt DataSourceErrorInfo' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DataSourceErrorInfo where
  rnf DataSourceErrorInfo' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'

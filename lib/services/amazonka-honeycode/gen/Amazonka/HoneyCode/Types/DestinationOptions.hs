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
-- Module      : Amazonka.HoneyCode.Types.DestinationOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.DestinationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types.SourceDataColumnProperties
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the options relating to the destination of the
-- import request.
--
-- /See:/ 'newDestinationOptions' smart constructor.
data DestinationOptions = DestinationOptions'
  { -- | A map of the column id to the import properties for each column.
    columnMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text SourceDataColumnProperties)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnMap', 'destinationOptions_columnMap' - A map of the column id to the import properties for each column.
newDestinationOptions ::
  DestinationOptions
newDestinationOptions =
  DestinationOptions' {columnMap = Prelude.Nothing}

-- | A map of the column id to the import properties for each column.
destinationOptions_columnMap :: Lens.Lens' DestinationOptions (Prelude.Maybe (Prelude.HashMap Prelude.Text SourceDataColumnProperties))
destinationOptions_columnMap = Lens.lens (\DestinationOptions' {columnMap} -> columnMap) (\s@DestinationOptions' {} a -> s {columnMap = a} :: DestinationOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DestinationOptions where
  parseJSON =
    Data.withObject
      "DestinationOptions"
      ( \x ->
          DestinationOptions'
            Prelude.<$> (x Data..:? "columnMap" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DestinationOptions where
  hashWithSalt _salt DestinationOptions' {..} =
    _salt `Prelude.hashWithSalt` columnMap

instance Prelude.NFData DestinationOptions where
  rnf DestinationOptions' {..} = Prelude.rnf columnMap

instance Data.ToJSON DestinationOptions where
  toJSON DestinationOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("columnMap" Data..=) Prelude.<$> columnMap]
      )

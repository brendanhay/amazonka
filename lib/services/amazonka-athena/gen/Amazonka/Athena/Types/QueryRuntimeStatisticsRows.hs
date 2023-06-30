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
-- Module      : Amazonka.Athena.Types.QueryRuntimeStatisticsRows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryRuntimeStatisticsRows where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statistics such as input rows and bytes read by the query, rows and
-- bytes output by the query, and the number of rows written by the query.
--
-- /See:/ 'newQueryRuntimeStatisticsRows' smart constructor.
data QueryRuntimeStatisticsRows = QueryRuntimeStatisticsRows'
  { -- | The number of bytes read to execute the query.
    inputBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of rows read to execute the query.
    inputRows :: Prelude.Maybe Prelude.Integer,
    -- | The number of bytes returned by the query.
    outputBytes :: Prelude.Maybe Prelude.Integer,
    -- | The number of rows returned by the query.
    outputRows :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryRuntimeStatisticsRows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputBytes', 'queryRuntimeStatisticsRows_inputBytes' - The number of bytes read to execute the query.
--
-- 'inputRows', 'queryRuntimeStatisticsRows_inputRows' - The number of rows read to execute the query.
--
-- 'outputBytes', 'queryRuntimeStatisticsRows_outputBytes' - The number of bytes returned by the query.
--
-- 'outputRows', 'queryRuntimeStatisticsRows_outputRows' - The number of rows returned by the query.
newQueryRuntimeStatisticsRows ::
  QueryRuntimeStatisticsRows
newQueryRuntimeStatisticsRows =
  QueryRuntimeStatisticsRows'
    { inputBytes =
        Prelude.Nothing,
      inputRows = Prelude.Nothing,
      outputBytes = Prelude.Nothing,
      outputRows = Prelude.Nothing
    }

-- | The number of bytes read to execute the query.
queryRuntimeStatisticsRows_inputBytes :: Lens.Lens' QueryRuntimeStatisticsRows (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsRows_inputBytes = Lens.lens (\QueryRuntimeStatisticsRows' {inputBytes} -> inputBytes) (\s@QueryRuntimeStatisticsRows' {} a -> s {inputBytes = a} :: QueryRuntimeStatisticsRows)

-- | The number of rows read to execute the query.
queryRuntimeStatisticsRows_inputRows :: Lens.Lens' QueryRuntimeStatisticsRows (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsRows_inputRows = Lens.lens (\QueryRuntimeStatisticsRows' {inputRows} -> inputRows) (\s@QueryRuntimeStatisticsRows' {} a -> s {inputRows = a} :: QueryRuntimeStatisticsRows)

-- | The number of bytes returned by the query.
queryRuntimeStatisticsRows_outputBytes :: Lens.Lens' QueryRuntimeStatisticsRows (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsRows_outputBytes = Lens.lens (\QueryRuntimeStatisticsRows' {outputBytes} -> outputBytes) (\s@QueryRuntimeStatisticsRows' {} a -> s {outputBytes = a} :: QueryRuntimeStatisticsRows)

-- | The number of rows returned by the query.
queryRuntimeStatisticsRows_outputRows :: Lens.Lens' QueryRuntimeStatisticsRows (Prelude.Maybe Prelude.Integer)
queryRuntimeStatisticsRows_outputRows = Lens.lens (\QueryRuntimeStatisticsRows' {outputRows} -> outputRows) (\s@QueryRuntimeStatisticsRows' {} a -> s {outputRows = a} :: QueryRuntimeStatisticsRows)

instance Data.FromJSON QueryRuntimeStatisticsRows where
  parseJSON =
    Data.withObject
      "QueryRuntimeStatisticsRows"
      ( \x ->
          QueryRuntimeStatisticsRows'
            Prelude.<$> (x Data..:? "InputBytes")
            Prelude.<*> (x Data..:? "InputRows")
            Prelude.<*> (x Data..:? "OutputBytes")
            Prelude.<*> (x Data..:? "OutputRows")
      )

instance Prelude.Hashable QueryRuntimeStatisticsRows where
  hashWithSalt _salt QueryRuntimeStatisticsRows' {..} =
    _salt
      `Prelude.hashWithSalt` inputBytes
      `Prelude.hashWithSalt` inputRows
      `Prelude.hashWithSalt` outputBytes
      `Prelude.hashWithSalt` outputRows

instance Prelude.NFData QueryRuntimeStatisticsRows where
  rnf QueryRuntimeStatisticsRows' {..} =
    Prelude.rnf inputBytes
      `Prelude.seq` Prelude.rnf inputRows
      `Prelude.seq` Prelude.rnf outputBytes
      `Prelude.seq` Prelude.rnf outputRows

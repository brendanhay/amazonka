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
-- Module      : Amazonka.DynamoDB.Types.TableClassSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TableClassSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TableClass
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Contains details of the table class.
--
-- /See:/ 'newTableClassSummary' smart constructor.
data TableClassSummary = TableClassSummary'
  { -- | The date and time at which the table class was last updated.
    lastUpdateDateTime :: Prelude.Maybe Data.POSIX,
    -- | The table class of the specified table. Valid values are @STANDARD@ and
    -- @STANDARD_INFREQUENT_ACCESS@.
    tableClass :: Prelude.Maybe TableClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableClassSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateDateTime', 'tableClassSummary_lastUpdateDateTime' - The date and time at which the table class was last updated.
--
-- 'tableClass', 'tableClassSummary_tableClass' - The table class of the specified table. Valid values are @STANDARD@ and
-- @STANDARD_INFREQUENT_ACCESS@.
newTableClassSummary ::
  TableClassSummary
newTableClassSummary =
  TableClassSummary'
    { lastUpdateDateTime =
        Prelude.Nothing,
      tableClass = Prelude.Nothing
    }

-- | The date and time at which the table class was last updated.
tableClassSummary_lastUpdateDateTime :: Lens.Lens' TableClassSummary (Prelude.Maybe Prelude.UTCTime)
tableClassSummary_lastUpdateDateTime = Lens.lens (\TableClassSummary' {lastUpdateDateTime} -> lastUpdateDateTime) (\s@TableClassSummary' {} a -> s {lastUpdateDateTime = a} :: TableClassSummary) Prelude.. Lens.mapping Data._Time

-- | The table class of the specified table. Valid values are @STANDARD@ and
-- @STANDARD_INFREQUENT_ACCESS@.
tableClassSummary_tableClass :: Lens.Lens' TableClassSummary (Prelude.Maybe TableClass)
tableClassSummary_tableClass = Lens.lens (\TableClassSummary' {tableClass} -> tableClass) (\s@TableClassSummary' {} a -> s {tableClass = a} :: TableClassSummary)

instance Data.FromJSON TableClassSummary where
  parseJSON =
    Data.withObject
      "TableClassSummary"
      ( \x ->
          TableClassSummary'
            Prelude.<$> (x Data..:? "LastUpdateDateTime")
            Prelude.<*> (x Data..:? "TableClass")
      )

instance Prelude.Hashable TableClassSummary where
  hashWithSalt _salt TableClassSummary' {..} =
    _salt
      `Prelude.hashWithSalt` lastUpdateDateTime
      `Prelude.hashWithSalt` tableClass

instance Prelude.NFData TableClassSummary where
  rnf TableClassSummary' {..} =
    Prelude.rnf lastUpdateDateTime `Prelude.seq`
      Prelude.rnf tableClass

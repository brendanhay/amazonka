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
-- Module      : Amazonka.DataPipeline.Types.Operator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.Operator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types.OperatorType
import qualified Amazonka.Prelude as Prelude

-- | Contains a logical operation for comparing the value of a field with a
-- specified value.
--
-- /See:/ 'newOperator' smart constructor.
data Operator = Operator'
  { -- | The logical operation to be performed: equal (@EQ@), equal reference
    -- (@REF_EQ@), less than or equal (@LE@), greater than or equal (@GE@), or
    -- between (@BETWEEN@). Equal reference (@REF_EQ@) can be used only with
    -- reference fields. The other comparison types can be used only with
    -- String fields. The comparison types you can use apply only to certain
    -- object fields, as detailed below.
    --
    -- The comparison operators EQ and REF_EQ act on the following fields:
    --
    -- -   name
    -- -   \@sphere
    -- -   parent
    -- -   \@componentParent
    -- -   \@instanceParent
    -- -   \@status
    -- -   \@scheduledStartTime
    -- -   \@scheduledEndTime
    -- -   \@actualStartTime
    -- -   \@actualEndTime
    --
    -- The comparison operators @GE@, @LE@, and @BETWEEN@ act on the following
    -- fields:
    --
    -- -   \@scheduledStartTime
    -- -   \@scheduledEndTime
    -- -   \@actualStartTime
    -- -   \@actualEndTime
    --
    -- Note that fields beginning with the at sign (\@) are read-only and set
    -- by the web service. When you name fields, you should choose names
    -- containing only alpha-numeric values, as symbols may be reserved by AWS
    -- Data Pipeline. User-defined fields that you add to a pipeline should
    -- prefix their name with the string \"my\".
    type' :: Prelude.Maybe OperatorType,
    -- | The value that the actual field value will be compared with.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Operator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'operator_type' - The logical operation to be performed: equal (@EQ@), equal reference
-- (@REF_EQ@), less than or equal (@LE@), greater than or equal (@GE@), or
-- between (@BETWEEN@). Equal reference (@REF_EQ@) can be used only with
-- reference fields. The other comparison types can be used only with
-- String fields. The comparison types you can use apply only to certain
-- object fields, as detailed below.
--
-- The comparison operators EQ and REF_EQ act on the following fields:
--
-- -   name
-- -   \@sphere
-- -   parent
-- -   \@componentParent
-- -   \@instanceParent
-- -   \@status
-- -   \@scheduledStartTime
-- -   \@scheduledEndTime
-- -   \@actualStartTime
-- -   \@actualEndTime
--
-- The comparison operators @GE@, @LE@, and @BETWEEN@ act on the following
-- fields:
--
-- -   \@scheduledStartTime
-- -   \@scheduledEndTime
-- -   \@actualStartTime
-- -   \@actualEndTime
--
-- Note that fields beginning with the at sign (\@) are read-only and set
-- by the web service. When you name fields, you should choose names
-- containing only alpha-numeric values, as symbols may be reserved by AWS
-- Data Pipeline. User-defined fields that you add to a pipeline should
-- prefix their name with the string \"my\".
--
-- 'values', 'operator_values' - The value that the actual field value will be compared with.
newOperator ::
  Operator
newOperator =
  Operator'
    { type' = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The logical operation to be performed: equal (@EQ@), equal reference
-- (@REF_EQ@), less than or equal (@LE@), greater than or equal (@GE@), or
-- between (@BETWEEN@). Equal reference (@REF_EQ@) can be used only with
-- reference fields. The other comparison types can be used only with
-- String fields. The comparison types you can use apply only to certain
-- object fields, as detailed below.
--
-- The comparison operators EQ and REF_EQ act on the following fields:
--
-- -   name
-- -   \@sphere
-- -   parent
-- -   \@componentParent
-- -   \@instanceParent
-- -   \@status
-- -   \@scheduledStartTime
-- -   \@scheduledEndTime
-- -   \@actualStartTime
-- -   \@actualEndTime
--
-- The comparison operators @GE@, @LE@, and @BETWEEN@ act on the following
-- fields:
--
-- -   \@scheduledStartTime
-- -   \@scheduledEndTime
-- -   \@actualStartTime
-- -   \@actualEndTime
--
-- Note that fields beginning with the at sign (\@) are read-only and set
-- by the web service. When you name fields, you should choose names
-- containing only alpha-numeric values, as symbols may be reserved by AWS
-- Data Pipeline. User-defined fields that you add to a pipeline should
-- prefix their name with the string \"my\".
operator_type :: Lens.Lens' Operator (Prelude.Maybe OperatorType)
operator_type = Lens.lens (\Operator' {type'} -> type') (\s@Operator' {} a -> s {type' = a} :: Operator)

-- | The value that the actual field value will be compared with.
operator_values :: Lens.Lens' Operator (Prelude.Maybe [Prelude.Text])
operator_values = Lens.lens (\Operator' {values} -> values) (\s@Operator' {} a -> s {values = a} :: Operator) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Operator where
  hashWithSalt _salt Operator' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` values

instance Prelude.NFData Operator where
  rnf Operator' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf values

instance Data.ToJSON Operator where
  toJSON Operator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("type" Data..=) Prelude.<$> type',
            ("values" Data..=) Prelude.<$> values
          ]
      )

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
-- Module      : Amazonka.KinesisVideo.Types.StreamNameCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.StreamNameCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.ComparisonOperator
import qualified Amazonka.Prelude as Prelude

-- | Specifies the condition that streams must satisfy to be returned when
-- you list streams (see the @ListStreams@ API). A condition has a
-- comparison operation and a value. Currently, you can specify only the
-- @BEGINS_WITH@ operator, which finds streams whose names start with a
-- given prefix.
--
-- /See:/ 'newStreamNameCondition' smart constructor.
data StreamNameCondition = StreamNameCondition'
  { -- | A value to compare.
    comparisonValue :: Prelude.Maybe Prelude.Text,
    -- | A comparison operator. Currently, you can specify only the @BEGINS_WITH@
    -- operator, which finds streams whose names start with a given prefix.
    comparisonOperator :: Prelude.Maybe ComparisonOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamNameCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparisonValue', 'streamNameCondition_comparisonValue' - A value to compare.
--
-- 'comparisonOperator', 'streamNameCondition_comparisonOperator' - A comparison operator. Currently, you can specify only the @BEGINS_WITH@
-- operator, which finds streams whose names start with a given prefix.
newStreamNameCondition ::
  StreamNameCondition
newStreamNameCondition =
  StreamNameCondition'
    { comparisonValue =
        Prelude.Nothing,
      comparisonOperator = Prelude.Nothing
    }

-- | A value to compare.
streamNameCondition_comparisonValue :: Lens.Lens' StreamNameCondition (Prelude.Maybe Prelude.Text)
streamNameCondition_comparisonValue = Lens.lens (\StreamNameCondition' {comparisonValue} -> comparisonValue) (\s@StreamNameCondition' {} a -> s {comparisonValue = a} :: StreamNameCondition)

-- | A comparison operator. Currently, you can specify only the @BEGINS_WITH@
-- operator, which finds streams whose names start with a given prefix.
streamNameCondition_comparisonOperator :: Lens.Lens' StreamNameCondition (Prelude.Maybe ComparisonOperator)
streamNameCondition_comparisonOperator = Lens.lens (\StreamNameCondition' {comparisonOperator} -> comparisonOperator) (\s@StreamNameCondition' {} a -> s {comparisonOperator = a} :: StreamNameCondition)

instance Prelude.Hashable StreamNameCondition where
  hashWithSalt _salt StreamNameCondition' {..} =
    _salt `Prelude.hashWithSalt` comparisonValue
      `Prelude.hashWithSalt` comparisonOperator

instance Prelude.NFData StreamNameCondition where
  rnf StreamNameCondition' {..} =
    Prelude.rnf comparisonValue
      `Prelude.seq` Prelude.rnf comparisonOperator

instance Data.ToJSON StreamNameCondition where
  toJSON StreamNameCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComparisonValue" Data..=)
              Prelude.<$> comparisonValue,
            ("ComparisonOperator" Data..=)
              Prelude.<$> comparisonOperator
          ]
      )

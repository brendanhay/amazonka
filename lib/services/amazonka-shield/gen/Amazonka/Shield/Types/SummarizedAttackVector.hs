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
-- Module      : Amazonka.Shield.Types.SummarizedAttackVector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.SummarizedAttackVector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.SummarizedCounter

-- | A summary of information about the attack.
--
-- /See:/ 'newSummarizedAttackVector' smart constructor.
data SummarizedAttackVector = SummarizedAttackVector'
  { -- | The list of counters that describe the details of the attack.
    vectorCounters :: Prelude.Maybe [SummarizedCounter],
    -- | The attack type, for example, SNMP reflection or SYN flood.
    vectorType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SummarizedAttackVector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vectorCounters', 'summarizedAttackVector_vectorCounters' - The list of counters that describe the details of the attack.
--
-- 'vectorType', 'summarizedAttackVector_vectorType' - The attack type, for example, SNMP reflection or SYN flood.
newSummarizedAttackVector ::
  -- | 'vectorType'
  Prelude.Text ->
  SummarizedAttackVector
newSummarizedAttackVector pVectorType_ =
  SummarizedAttackVector'
    { vectorCounters =
        Prelude.Nothing,
      vectorType = pVectorType_
    }

-- | The list of counters that describe the details of the attack.
summarizedAttackVector_vectorCounters :: Lens.Lens' SummarizedAttackVector (Prelude.Maybe [SummarizedCounter])
summarizedAttackVector_vectorCounters = Lens.lens (\SummarizedAttackVector' {vectorCounters} -> vectorCounters) (\s@SummarizedAttackVector' {} a -> s {vectorCounters = a} :: SummarizedAttackVector) Prelude.. Lens.mapping Lens.coerced

-- | The attack type, for example, SNMP reflection or SYN flood.
summarizedAttackVector_vectorType :: Lens.Lens' SummarizedAttackVector Prelude.Text
summarizedAttackVector_vectorType = Lens.lens (\SummarizedAttackVector' {vectorType} -> vectorType) (\s@SummarizedAttackVector' {} a -> s {vectorType = a} :: SummarizedAttackVector)

instance Data.FromJSON SummarizedAttackVector where
  parseJSON =
    Data.withObject
      "SummarizedAttackVector"
      ( \x ->
          SummarizedAttackVector'
            Prelude.<$> (x Data..:? "VectorCounters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "VectorType")
      )

instance Prelude.Hashable SummarizedAttackVector where
  hashWithSalt _salt SummarizedAttackVector' {..} =
    _salt
      `Prelude.hashWithSalt` vectorCounters
      `Prelude.hashWithSalt` vectorType

instance Prelude.NFData SummarizedAttackVector where
  rnf SummarizedAttackVector' {..} =
    Prelude.rnf vectorCounters
      `Prelude.seq` Prelude.rnf vectorType

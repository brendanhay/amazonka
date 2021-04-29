{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.Types.SummarizedAttackVector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SummarizedAttackVector where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.SummarizedCounter

-- | A summary of information about the attack.
--
-- /See:/ 'newSummarizedAttackVector' smart constructor.
data SummarizedAttackVector = SummarizedAttackVector'
  { -- | The list of counters that describe the details of the attack.
    vectorCounters :: Prelude.Maybe [SummarizedCounter],
    -- | The attack type, for example, SNMP reflection or SYN flood.
    vectorType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
summarizedAttackVector_vectorCounters = Lens.lens (\SummarizedAttackVector' {vectorCounters} -> vectorCounters) (\s@SummarizedAttackVector' {} a -> s {vectorCounters = a} :: SummarizedAttackVector) Prelude.. Lens.mapping Prelude._Coerce

-- | The attack type, for example, SNMP reflection or SYN flood.
summarizedAttackVector_vectorType :: Lens.Lens' SummarizedAttackVector Prelude.Text
summarizedAttackVector_vectorType = Lens.lens (\SummarizedAttackVector' {vectorType} -> vectorType) (\s@SummarizedAttackVector' {} a -> s {vectorType = a} :: SummarizedAttackVector)

instance Prelude.FromJSON SummarizedAttackVector where
  parseJSON =
    Prelude.withObject
      "SummarizedAttackVector"
      ( \x ->
          SummarizedAttackVector'
            Prelude.<$> ( x Prelude..:? "VectorCounters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "VectorType")
      )

instance Prelude.Hashable SummarizedAttackVector

instance Prelude.NFData SummarizedAttackVector

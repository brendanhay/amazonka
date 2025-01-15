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
-- Module      : Amazonka.SageMaker.Types.AlgorithmSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AlgorithmSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AlgorithmStatus

-- | Provides summary information about an algorithm.
--
-- /See:/ 'newAlgorithmSummary' smart constructor.
data AlgorithmSummary = AlgorithmSummary'
  { -- | A brief description of the algorithm.
    algorithmDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the algorithm that is described by the summary.
    algorithmName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the algorithm.
    algorithmArn :: Prelude.Text,
    -- | A timestamp that shows when the algorithm was created.
    creationTime :: Data.POSIX,
    -- | The overall status of the algorithm.
    algorithmStatus :: AlgorithmStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmDescription', 'algorithmSummary_algorithmDescription' - A brief description of the algorithm.
--
-- 'algorithmName', 'algorithmSummary_algorithmName' - The name of the algorithm that is described by the summary.
--
-- 'algorithmArn', 'algorithmSummary_algorithmArn' - The Amazon Resource Name (ARN) of the algorithm.
--
-- 'creationTime', 'algorithmSummary_creationTime' - A timestamp that shows when the algorithm was created.
--
-- 'algorithmStatus', 'algorithmSummary_algorithmStatus' - The overall status of the algorithm.
newAlgorithmSummary ::
  -- | 'algorithmName'
  Prelude.Text ->
  -- | 'algorithmArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'algorithmStatus'
  AlgorithmStatus ->
  AlgorithmSummary
newAlgorithmSummary
  pAlgorithmName_
  pAlgorithmArn_
  pCreationTime_
  pAlgorithmStatus_ =
    AlgorithmSummary'
      { algorithmDescription =
          Prelude.Nothing,
        algorithmName = pAlgorithmName_,
        algorithmArn = pAlgorithmArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        algorithmStatus = pAlgorithmStatus_
      }

-- | A brief description of the algorithm.
algorithmSummary_algorithmDescription :: Lens.Lens' AlgorithmSummary (Prelude.Maybe Prelude.Text)
algorithmSummary_algorithmDescription = Lens.lens (\AlgorithmSummary' {algorithmDescription} -> algorithmDescription) (\s@AlgorithmSummary' {} a -> s {algorithmDescription = a} :: AlgorithmSummary)

-- | The name of the algorithm that is described by the summary.
algorithmSummary_algorithmName :: Lens.Lens' AlgorithmSummary Prelude.Text
algorithmSummary_algorithmName = Lens.lens (\AlgorithmSummary' {algorithmName} -> algorithmName) (\s@AlgorithmSummary' {} a -> s {algorithmName = a} :: AlgorithmSummary)

-- | The Amazon Resource Name (ARN) of the algorithm.
algorithmSummary_algorithmArn :: Lens.Lens' AlgorithmSummary Prelude.Text
algorithmSummary_algorithmArn = Lens.lens (\AlgorithmSummary' {algorithmArn} -> algorithmArn) (\s@AlgorithmSummary' {} a -> s {algorithmArn = a} :: AlgorithmSummary)

-- | A timestamp that shows when the algorithm was created.
algorithmSummary_creationTime :: Lens.Lens' AlgorithmSummary Prelude.UTCTime
algorithmSummary_creationTime = Lens.lens (\AlgorithmSummary' {creationTime} -> creationTime) (\s@AlgorithmSummary' {} a -> s {creationTime = a} :: AlgorithmSummary) Prelude.. Data._Time

-- | The overall status of the algorithm.
algorithmSummary_algorithmStatus :: Lens.Lens' AlgorithmSummary AlgorithmStatus
algorithmSummary_algorithmStatus = Lens.lens (\AlgorithmSummary' {algorithmStatus} -> algorithmStatus) (\s@AlgorithmSummary' {} a -> s {algorithmStatus = a} :: AlgorithmSummary)

instance Data.FromJSON AlgorithmSummary where
  parseJSON =
    Data.withObject
      "AlgorithmSummary"
      ( \x ->
          AlgorithmSummary'
            Prelude.<$> (x Data..:? "AlgorithmDescription")
            Prelude.<*> (x Data..: "AlgorithmName")
            Prelude.<*> (x Data..: "AlgorithmArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "AlgorithmStatus")
      )

instance Prelude.Hashable AlgorithmSummary where
  hashWithSalt _salt AlgorithmSummary' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmDescription
      `Prelude.hashWithSalt` algorithmName
      `Prelude.hashWithSalt` algorithmArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` algorithmStatus

instance Prelude.NFData AlgorithmSummary where
  rnf AlgorithmSummary' {..} =
    Prelude.rnf algorithmDescription `Prelude.seq`
      Prelude.rnf algorithmName `Prelude.seq`
        Prelude.rnf algorithmArn `Prelude.seq`
          Prelude.rnf creationTime `Prelude.seq`
            Prelude.rnf algorithmStatus

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
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AlgorithmStatus

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
    creationTime :: Prelude.POSIX,
    -- | The overall status of the algorithm.
    algorithmStatus :: AlgorithmStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        creationTime = Prelude._Time Lens.# pCreationTime_,
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
algorithmSummary_creationTime = Lens.lens (\AlgorithmSummary' {creationTime} -> creationTime) (\s@AlgorithmSummary' {} a -> s {creationTime = a} :: AlgorithmSummary) Prelude.. Prelude._Time

-- | The overall status of the algorithm.
algorithmSummary_algorithmStatus :: Lens.Lens' AlgorithmSummary AlgorithmStatus
algorithmSummary_algorithmStatus = Lens.lens (\AlgorithmSummary' {algorithmStatus} -> algorithmStatus) (\s@AlgorithmSummary' {} a -> s {algorithmStatus = a} :: AlgorithmSummary)

instance Prelude.FromJSON AlgorithmSummary where
  parseJSON =
    Prelude.withObject
      "AlgorithmSummary"
      ( \x ->
          AlgorithmSummary'
            Prelude.<$> (x Prelude..:? "AlgorithmDescription")
            Prelude.<*> (x Prelude..: "AlgorithmName")
            Prelude.<*> (x Prelude..: "AlgorithmArn")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "AlgorithmStatus")
      )

instance Prelude.Hashable AlgorithmSummary

instance Prelude.NFData AlgorithmSummary

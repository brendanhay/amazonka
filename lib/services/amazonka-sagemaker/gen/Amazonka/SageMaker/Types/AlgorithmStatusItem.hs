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
-- Module      : Amazonka.SageMaker.Types.AlgorithmStatusItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AlgorithmStatusItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DetailedAlgorithmStatus

-- | Represents the overall status of an algorithm.
--
-- /See:/ 'newAlgorithmStatusItem' smart constructor.
data AlgorithmStatusItem = AlgorithmStatusItem'
  { -- | if the overall status is @Failed@, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the algorithm for which the overall status is being
    -- reported.
    name :: Prelude.Text,
    -- | The current status.
    status :: DetailedAlgorithmStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmStatusItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'algorithmStatusItem_failureReason' - if the overall status is @Failed@, the reason for the failure.
--
-- 'name', 'algorithmStatusItem_name' - The name of the algorithm for which the overall status is being
-- reported.
--
-- 'status', 'algorithmStatusItem_status' - The current status.
newAlgorithmStatusItem ::
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  DetailedAlgorithmStatus ->
  AlgorithmStatusItem
newAlgorithmStatusItem pName_ pStatus_ =
  AlgorithmStatusItem'
    { failureReason =
        Prelude.Nothing,
      name = pName_,
      status = pStatus_
    }

-- | if the overall status is @Failed@, the reason for the failure.
algorithmStatusItem_failureReason :: Lens.Lens' AlgorithmStatusItem (Prelude.Maybe Prelude.Text)
algorithmStatusItem_failureReason = Lens.lens (\AlgorithmStatusItem' {failureReason} -> failureReason) (\s@AlgorithmStatusItem' {} a -> s {failureReason = a} :: AlgorithmStatusItem)

-- | The name of the algorithm for which the overall status is being
-- reported.
algorithmStatusItem_name :: Lens.Lens' AlgorithmStatusItem Prelude.Text
algorithmStatusItem_name = Lens.lens (\AlgorithmStatusItem' {name} -> name) (\s@AlgorithmStatusItem' {} a -> s {name = a} :: AlgorithmStatusItem)

-- | The current status.
algorithmStatusItem_status :: Lens.Lens' AlgorithmStatusItem DetailedAlgorithmStatus
algorithmStatusItem_status = Lens.lens (\AlgorithmStatusItem' {status} -> status) (\s@AlgorithmStatusItem' {} a -> s {status = a} :: AlgorithmStatusItem)

instance Data.FromJSON AlgorithmStatusItem where
  parseJSON =
    Data.withObject
      "AlgorithmStatusItem"
      ( \x ->
          AlgorithmStatusItem'
            Prelude.<$> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable AlgorithmStatusItem where
  hashWithSalt _salt AlgorithmStatusItem' {..} =
    _salt
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData AlgorithmStatusItem where
  rnf AlgorithmStatusItem' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status

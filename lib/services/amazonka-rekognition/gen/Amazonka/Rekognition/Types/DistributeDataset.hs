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
-- Module      : Amazonka.Rekognition.Types.DistributeDataset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DistributeDataset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A training dataset or a test dataset used in a dataset distribution
-- operation. For more information, see DistributeDatasetEntries.
--
-- /See:/ 'newDistributeDataset' smart constructor.
data DistributeDataset = DistributeDataset'
  { -- | The Amazon Resource Name (ARN) of the dataset that you want to use.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributeDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'distributeDataset_arn' - The Amazon Resource Name (ARN) of the dataset that you want to use.
newDistributeDataset ::
  -- | 'arn'
  Prelude.Text ->
  DistributeDataset
newDistributeDataset pArn_ =
  DistributeDataset' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the dataset that you want to use.
distributeDataset_arn :: Lens.Lens' DistributeDataset Prelude.Text
distributeDataset_arn = Lens.lens (\DistributeDataset' {arn} -> arn) (\s@DistributeDataset' {} a -> s {arn = a} :: DistributeDataset)

instance Prelude.Hashable DistributeDataset where
  hashWithSalt _salt DistributeDataset' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DistributeDataset where
  rnf DistributeDataset' {..} = Prelude.rnf arn

instance Core.ToJSON DistributeDataset where
  toJSON DistributeDataset' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Core..= arn)]
      )

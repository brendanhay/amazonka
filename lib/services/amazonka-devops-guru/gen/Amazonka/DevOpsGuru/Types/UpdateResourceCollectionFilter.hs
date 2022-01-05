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
-- Module      : Amazonka.DevOpsGuru.Types.UpdateResourceCollectionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.UpdateResourceCollectionFilter where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.UpdateCloudFormationCollectionFilter
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information used to update a collection of AWS resources.
--
-- /See:/ 'newUpdateResourceCollectionFilter' smart constructor.
data UpdateResourceCollectionFilter = UpdateResourceCollectionFilter'
  { -- | An collection of AWS CloudFormation stacks. You can specify up to 500
    -- AWS CloudFormation stacks.
    cloudFormation :: Prelude.Maybe UpdateCloudFormationCollectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormation', 'updateResourceCollectionFilter_cloudFormation' - An collection of AWS CloudFormation stacks. You can specify up to 500
-- AWS CloudFormation stacks.
newUpdateResourceCollectionFilter ::
  UpdateResourceCollectionFilter
newUpdateResourceCollectionFilter =
  UpdateResourceCollectionFilter'
    { cloudFormation =
        Prelude.Nothing
    }

-- | An collection of AWS CloudFormation stacks. You can specify up to 500
-- AWS CloudFormation stacks.
updateResourceCollectionFilter_cloudFormation :: Lens.Lens' UpdateResourceCollectionFilter (Prelude.Maybe UpdateCloudFormationCollectionFilter)
updateResourceCollectionFilter_cloudFormation = Lens.lens (\UpdateResourceCollectionFilter' {cloudFormation} -> cloudFormation) (\s@UpdateResourceCollectionFilter' {} a -> s {cloudFormation = a} :: UpdateResourceCollectionFilter)

instance
  Prelude.Hashable
    UpdateResourceCollectionFilter
  where
  hashWithSalt
    _salt
    UpdateResourceCollectionFilter' {..} =
      _salt `Prelude.hashWithSalt` cloudFormation

instance
  Prelude.NFData
    UpdateResourceCollectionFilter
  where
  rnf UpdateResourceCollectionFilter' {..} =
    Prelude.rnf cloudFormation

instance Core.ToJSON UpdateResourceCollectionFilter where
  toJSON UpdateResourceCollectionFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CloudFormation" Core..=)
              Prelude.<$> cloudFormation
          ]
      )

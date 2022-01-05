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
-- Module      : Amazonka.DevOpsGuru.Types.ResourceCollectionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ResourceCollectionFilter where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.CloudFormationCollectionFilter
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a filter used to specify which AWS resources are
-- analyzed for anomalous behavior by DevOps Guru.
--
-- /See:/ 'newResourceCollectionFilter' smart constructor.
data ResourceCollectionFilter = ResourceCollectionFilter'
  { -- | Information about AWS CloudFormation stacks. You can use up to 500
    -- stacks to specify which AWS resources in your account to analyze. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
    -- in the /AWS CloudFormation User Guide/.
    cloudFormation :: Prelude.Maybe CloudFormationCollectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormation', 'resourceCollectionFilter_cloudFormation' - Information about AWS CloudFormation stacks. You can use up to 500
-- stacks to specify which AWS resources in your account to analyze. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
-- in the /AWS CloudFormation User Guide/.
newResourceCollectionFilter ::
  ResourceCollectionFilter
newResourceCollectionFilter =
  ResourceCollectionFilter'
    { cloudFormation =
        Prelude.Nothing
    }

-- | Information about AWS CloudFormation stacks. You can use up to 500
-- stacks to specify which AWS resources in your account to analyze. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
-- in the /AWS CloudFormation User Guide/.
resourceCollectionFilter_cloudFormation :: Lens.Lens' ResourceCollectionFilter (Prelude.Maybe CloudFormationCollectionFilter)
resourceCollectionFilter_cloudFormation = Lens.lens (\ResourceCollectionFilter' {cloudFormation} -> cloudFormation) (\s@ResourceCollectionFilter' {} a -> s {cloudFormation = a} :: ResourceCollectionFilter)

instance Core.FromJSON ResourceCollectionFilter where
  parseJSON =
    Core.withObject
      "ResourceCollectionFilter"
      ( \x ->
          ResourceCollectionFilter'
            Prelude.<$> (x Core..:? "CloudFormation")
      )

instance Prelude.Hashable ResourceCollectionFilter where
  hashWithSalt _salt ResourceCollectionFilter' {..} =
    _salt `Prelude.hashWithSalt` cloudFormation

instance Prelude.NFData ResourceCollectionFilter where
  rnf ResourceCollectionFilter' {..} =
    Prelude.rnf cloudFormation

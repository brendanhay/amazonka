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
-- Module      : Amazonka.AutoScalingPlans.Types.ApplicationSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ApplicationSource where

import Amazonka.AutoScalingPlans.Types.TagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an application source.
--
-- /See:/ 'newApplicationSource' smart constructor.
data ApplicationSource = ApplicationSource'
  { -- | A set of tags (up to 50).
    tagFilters :: Prelude.Maybe [TagFilter],
    -- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
    cloudFormationStackARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilters', 'applicationSource_tagFilters' - A set of tags (up to 50).
--
-- 'cloudFormationStackARN', 'applicationSource_cloudFormationStackARN' - The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
newApplicationSource ::
  ApplicationSource
newApplicationSource =
  ApplicationSource'
    { tagFilters = Prelude.Nothing,
      cloudFormationStackARN = Prelude.Nothing
    }

-- | A set of tags (up to 50).
applicationSource_tagFilters :: Lens.Lens' ApplicationSource (Prelude.Maybe [TagFilter])
applicationSource_tagFilters = Lens.lens (\ApplicationSource' {tagFilters} -> tagFilters) (\s@ApplicationSource' {} a -> s {tagFilters = a} :: ApplicationSource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
applicationSource_cloudFormationStackARN :: Lens.Lens' ApplicationSource (Prelude.Maybe Prelude.Text)
applicationSource_cloudFormationStackARN = Lens.lens (\ApplicationSource' {cloudFormationStackARN} -> cloudFormationStackARN) (\s@ApplicationSource' {} a -> s {cloudFormationStackARN = a} :: ApplicationSource)

instance Core.FromJSON ApplicationSource where
  parseJSON =
    Core.withObject
      "ApplicationSource"
      ( \x ->
          ApplicationSource'
            Prelude.<$> (x Core..:? "TagFilters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CloudFormationStackARN")
      )

instance Prelude.Hashable ApplicationSource where
  hashWithSalt salt' ApplicationSource' {..} =
    salt' `Prelude.hashWithSalt` cloudFormationStackARN
      `Prelude.hashWithSalt` tagFilters

instance Prelude.NFData ApplicationSource where
  rnf ApplicationSource' {..} =
    Prelude.rnf tagFilters
      `Prelude.seq` Prelude.rnf cloudFormationStackARN

instance Core.ToJSON ApplicationSource where
  toJSON ApplicationSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TagFilters" Core..=) Prelude.<$> tagFilters,
            ("CloudFormationStackARN" Core..=)
              Prelude.<$> cloudFormationStackARN
          ]
      )

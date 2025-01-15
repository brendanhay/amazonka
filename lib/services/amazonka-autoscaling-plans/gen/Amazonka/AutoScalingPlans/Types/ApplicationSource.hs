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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ApplicationSource where

import Amazonka.AutoScalingPlans.Types.TagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an application source.
--
-- /See:/ 'newApplicationSource' smart constructor.
data ApplicationSource = ApplicationSource'
  { -- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
    cloudFormationStackARN :: Prelude.Maybe Prelude.Text,
    -- | A set of tags (up to 50).
    tagFilters :: Prelude.Maybe [TagFilter]
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
-- 'cloudFormationStackARN', 'applicationSource_cloudFormationStackARN' - The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
--
-- 'tagFilters', 'applicationSource_tagFilters' - A set of tags (up to 50).
newApplicationSource ::
  ApplicationSource
newApplicationSource =
  ApplicationSource'
    { cloudFormationStackARN =
        Prelude.Nothing,
      tagFilters = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
applicationSource_cloudFormationStackARN :: Lens.Lens' ApplicationSource (Prelude.Maybe Prelude.Text)
applicationSource_cloudFormationStackARN = Lens.lens (\ApplicationSource' {cloudFormationStackARN} -> cloudFormationStackARN) (\s@ApplicationSource' {} a -> s {cloudFormationStackARN = a} :: ApplicationSource)

-- | A set of tags (up to 50).
applicationSource_tagFilters :: Lens.Lens' ApplicationSource (Prelude.Maybe [TagFilter])
applicationSource_tagFilters = Lens.lens (\ApplicationSource' {tagFilters} -> tagFilters) (\s@ApplicationSource' {} a -> s {tagFilters = a} :: ApplicationSource) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ApplicationSource where
  parseJSON =
    Data.withObject
      "ApplicationSource"
      ( \x ->
          ApplicationSource'
            Prelude.<$> (x Data..:? "CloudFormationStackARN")
            Prelude.<*> (x Data..:? "TagFilters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ApplicationSource where
  hashWithSalt _salt ApplicationSource' {..} =
    _salt
      `Prelude.hashWithSalt` cloudFormationStackARN
      `Prelude.hashWithSalt` tagFilters

instance Prelude.NFData ApplicationSource where
  rnf ApplicationSource' {..} =
    Prelude.rnf cloudFormationStackARN `Prelude.seq`
      Prelude.rnf tagFilters

instance Data.ToJSON ApplicationSource where
  toJSON ApplicationSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudFormationStackARN" Data..=)
              Prelude.<$> cloudFormationStackARN,
            ("TagFilters" Data..=) Prelude.<$> tagFilters
          ]
      )

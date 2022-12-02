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
-- Module      : Amazonka.DevOpsGuru.Types.CloudFormationCollection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CloudFormationCollection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about Amazon Web Services CloudFormation stacks. You can use
-- up to 500 stacks to specify which Amazon Web Services resources in your
-- account to analyze. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
-- in the /Amazon Web Services CloudFormation User Guide/.
--
-- /See:/ 'newCloudFormationCollection' smart constructor.
data CloudFormationCollection = CloudFormationCollection'
  { -- | An array of CloudFormation stack names.
    stackNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudFormationCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackNames', 'cloudFormationCollection_stackNames' - An array of CloudFormation stack names.
newCloudFormationCollection ::
  CloudFormationCollection
newCloudFormationCollection =
  CloudFormationCollection'
    { stackNames =
        Prelude.Nothing
    }

-- | An array of CloudFormation stack names.
cloudFormationCollection_stackNames :: Lens.Lens' CloudFormationCollection (Prelude.Maybe [Prelude.Text])
cloudFormationCollection_stackNames = Lens.lens (\CloudFormationCollection' {stackNames} -> stackNames) (\s@CloudFormationCollection' {} a -> s {stackNames = a} :: CloudFormationCollection) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CloudFormationCollection where
  parseJSON =
    Data.withObject
      "CloudFormationCollection"
      ( \x ->
          CloudFormationCollection'
            Prelude.<$> (x Data..:? "StackNames" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CloudFormationCollection where
  hashWithSalt _salt CloudFormationCollection' {..} =
    _salt `Prelude.hashWithSalt` stackNames

instance Prelude.NFData CloudFormationCollection where
  rnf CloudFormationCollection' {..} =
    Prelude.rnf stackNames

instance Data.ToJSON CloudFormationCollection where
  toJSON CloudFormationCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StackNames" Data..=) Prelude.<$> stackNames]
      )

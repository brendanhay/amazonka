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
-- Module      : Network.AWS.DataBrew.Types.PathOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataBrew.Types.PathOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.DataBrew.Types.DatasetParameter
import Network.AWS.DataBrew.Types.FilesLimit
import Network.AWS.DataBrew.Types.FilterExpression
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a set of options that define how DataBrew selects files for a
-- given Amazon S3 path in a dataset.
--
-- /See:/ 'newPathOptions' smart constructor.
data PathOptions = PathOptions'
  { -- | If provided, this structure defines a date range for matching Amazon S3
    -- objects based on their LastModifiedDate attribute in Amazon S3.
    lastModifiedDateCondition :: Prelude.Maybe FilterExpression,
    -- | A structure that maps names of parameters used in the Amazon S3 path of
    -- a dataset to their definitions.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text DatasetParameter),
    -- | If provided, this structure imposes a limit on a number of files that
    -- should be selected.
    filesLimit :: Prelude.Maybe FilesLimit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDateCondition', 'pathOptions_lastModifiedDateCondition' - If provided, this structure defines a date range for matching Amazon S3
-- objects based on their LastModifiedDate attribute in Amazon S3.
--
-- 'parameters', 'pathOptions_parameters' - A structure that maps names of parameters used in the Amazon S3 path of
-- a dataset to their definitions.
--
-- 'filesLimit', 'pathOptions_filesLimit' - If provided, this structure imposes a limit on a number of files that
-- should be selected.
newPathOptions ::
  PathOptions
newPathOptions =
  PathOptions'
    { lastModifiedDateCondition =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      filesLimit = Prelude.Nothing
    }

-- | If provided, this structure defines a date range for matching Amazon S3
-- objects based on their LastModifiedDate attribute in Amazon S3.
pathOptions_lastModifiedDateCondition :: Lens.Lens' PathOptions (Prelude.Maybe FilterExpression)
pathOptions_lastModifiedDateCondition = Lens.lens (\PathOptions' {lastModifiedDateCondition} -> lastModifiedDateCondition) (\s@PathOptions' {} a -> s {lastModifiedDateCondition = a} :: PathOptions)

-- | A structure that maps names of parameters used in the Amazon S3 path of
-- a dataset to their definitions.
pathOptions_parameters :: Lens.Lens' PathOptions (Prelude.Maybe (Prelude.HashMap Prelude.Text DatasetParameter))
pathOptions_parameters = Lens.lens (\PathOptions' {parameters} -> parameters) (\s@PathOptions' {} a -> s {parameters = a} :: PathOptions) Prelude.. Lens.mapping Lens.coerced

-- | If provided, this structure imposes a limit on a number of files that
-- should be selected.
pathOptions_filesLimit :: Lens.Lens' PathOptions (Prelude.Maybe FilesLimit)
pathOptions_filesLimit = Lens.lens (\PathOptions' {filesLimit} -> filesLimit) (\s@PathOptions' {} a -> s {filesLimit = a} :: PathOptions)

instance Core.FromJSON PathOptions where
  parseJSON =
    Core.withObject
      "PathOptions"
      ( \x ->
          PathOptions'
            Prelude.<$> (x Core..:? "LastModifiedDateCondition")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "FilesLimit")
      )

instance Prelude.Hashable PathOptions

instance Prelude.NFData PathOptions

instance Core.ToJSON PathOptions where
  toJSON PathOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LastModifiedDateCondition" Core..=)
              Prelude.<$> lastModifiedDateCondition,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("FilesLimit" Core..=) Prelude.<$> filesLimit
          ]
      )

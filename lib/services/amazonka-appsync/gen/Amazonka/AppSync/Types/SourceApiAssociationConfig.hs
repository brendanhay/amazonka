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
-- Module      : Amazonka.AppSync.Types.SourceApiAssociationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.SourceApiAssociationConfig where

import Amazonka.AppSync.Types.MergeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes properties used to specify configurations related to a source
-- API.
--
-- /See:/ 'newSourceApiAssociationConfig' smart constructor.
data SourceApiAssociationConfig = SourceApiAssociationConfig'
  { -- | The property that indicates which merging option is enabled in the
    -- source API association.
    --
    -- Valid merge types are @MANUAL_MERGE@ (default) and @AUTO_MERGE@. Manual
    -- merges are the default behavior and require the user to trigger any
    -- changes from the source APIs to the merged API manually. Auto merges
    -- subscribe the merged API to the changes performed on the source APIs so
    -- that any change in the source APIs are also made to the merged API. Auto
    -- merges use @MergedApiExecutionRoleArn@ to perform merge operations.
    mergeType :: Prelude.Maybe MergeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceApiAssociationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mergeType', 'sourceApiAssociationConfig_mergeType' - The property that indicates which merging option is enabled in the
-- source API association.
--
-- Valid merge types are @MANUAL_MERGE@ (default) and @AUTO_MERGE@. Manual
-- merges are the default behavior and require the user to trigger any
-- changes from the source APIs to the merged API manually. Auto merges
-- subscribe the merged API to the changes performed on the source APIs so
-- that any change in the source APIs are also made to the merged API. Auto
-- merges use @MergedApiExecutionRoleArn@ to perform merge operations.
newSourceApiAssociationConfig ::
  SourceApiAssociationConfig
newSourceApiAssociationConfig =
  SourceApiAssociationConfig'
    { mergeType =
        Prelude.Nothing
    }

-- | The property that indicates which merging option is enabled in the
-- source API association.
--
-- Valid merge types are @MANUAL_MERGE@ (default) and @AUTO_MERGE@. Manual
-- merges are the default behavior and require the user to trigger any
-- changes from the source APIs to the merged API manually. Auto merges
-- subscribe the merged API to the changes performed on the source APIs so
-- that any change in the source APIs are also made to the merged API. Auto
-- merges use @MergedApiExecutionRoleArn@ to perform merge operations.
sourceApiAssociationConfig_mergeType :: Lens.Lens' SourceApiAssociationConfig (Prelude.Maybe MergeType)
sourceApiAssociationConfig_mergeType = Lens.lens (\SourceApiAssociationConfig' {mergeType} -> mergeType) (\s@SourceApiAssociationConfig' {} a -> s {mergeType = a} :: SourceApiAssociationConfig)

instance Data.FromJSON SourceApiAssociationConfig where
  parseJSON =
    Data.withObject
      "SourceApiAssociationConfig"
      ( \x ->
          SourceApiAssociationConfig'
            Prelude.<$> (x Data..:? "mergeType")
      )

instance Prelude.Hashable SourceApiAssociationConfig where
  hashWithSalt _salt SourceApiAssociationConfig' {..} =
    _salt `Prelude.hashWithSalt` mergeType

instance Prelude.NFData SourceApiAssociationConfig where
  rnf SourceApiAssociationConfig' {..} =
    Prelude.rnf mergeType

instance Data.ToJSON SourceApiAssociationConfig where
  toJSON SourceApiAssociationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("mergeType" Data..=) Prelude.<$> mergeType]
      )

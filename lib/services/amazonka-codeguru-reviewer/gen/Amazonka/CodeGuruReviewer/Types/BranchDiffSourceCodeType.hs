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
-- Module      : Amazonka.CodeGuruReviewer.Types.BranchDiffSourceCodeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.BranchDiffSourceCodeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A type of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies a code diff between a source and destination branch in an
-- associated repository.
--
-- /See:/ 'newBranchDiffSourceCodeType' smart constructor.
data BranchDiffSourceCodeType = BranchDiffSourceCodeType'
  { -- | The source branch for a diff in an associated repository.
    sourceBranchName :: Prelude.Text,
    -- | The destination branch for a diff in an associated repository.
    destinationBranchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BranchDiffSourceCodeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceBranchName', 'branchDiffSourceCodeType_sourceBranchName' - The source branch for a diff in an associated repository.
--
-- 'destinationBranchName', 'branchDiffSourceCodeType_destinationBranchName' - The destination branch for a diff in an associated repository.
newBranchDiffSourceCodeType ::
  -- | 'sourceBranchName'
  Prelude.Text ->
  -- | 'destinationBranchName'
  Prelude.Text ->
  BranchDiffSourceCodeType
newBranchDiffSourceCodeType
  pSourceBranchName_
  pDestinationBranchName_ =
    BranchDiffSourceCodeType'
      { sourceBranchName =
          pSourceBranchName_,
        destinationBranchName = pDestinationBranchName_
      }

-- | The source branch for a diff in an associated repository.
branchDiffSourceCodeType_sourceBranchName :: Lens.Lens' BranchDiffSourceCodeType Prelude.Text
branchDiffSourceCodeType_sourceBranchName = Lens.lens (\BranchDiffSourceCodeType' {sourceBranchName} -> sourceBranchName) (\s@BranchDiffSourceCodeType' {} a -> s {sourceBranchName = a} :: BranchDiffSourceCodeType)

-- | The destination branch for a diff in an associated repository.
branchDiffSourceCodeType_destinationBranchName :: Lens.Lens' BranchDiffSourceCodeType Prelude.Text
branchDiffSourceCodeType_destinationBranchName = Lens.lens (\BranchDiffSourceCodeType' {destinationBranchName} -> destinationBranchName) (\s@BranchDiffSourceCodeType' {} a -> s {destinationBranchName = a} :: BranchDiffSourceCodeType)

instance Core.FromJSON BranchDiffSourceCodeType where
  parseJSON =
    Core.withObject
      "BranchDiffSourceCodeType"
      ( \x ->
          BranchDiffSourceCodeType'
            Prelude.<$> (x Core..: "SourceBranchName")
            Prelude.<*> (x Core..: "DestinationBranchName")
      )

instance Prelude.Hashable BranchDiffSourceCodeType where
  hashWithSalt _salt BranchDiffSourceCodeType' {..} =
    _salt `Prelude.hashWithSalt` sourceBranchName
      `Prelude.hashWithSalt` destinationBranchName

instance Prelude.NFData BranchDiffSourceCodeType where
  rnf BranchDiffSourceCodeType' {..} =
    Prelude.rnf sourceBranchName
      `Prelude.seq` Prelude.rnf destinationBranchName

instance Core.ToJSON BranchDiffSourceCodeType where
  toJSON BranchDiffSourceCodeType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SourceBranchName" Core..= sourceBranchName),
            Prelude.Just
              ( "DestinationBranchName"
                  Core..= destinationBranchName
              )
          ]
      )

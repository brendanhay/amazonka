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
-- Module      : Amazonka.CodeStar.Types.ToolchainSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.ToolchainSource where

import Amazonka.CodeStar.Types.S3Location
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 location where the toolchain template file provided with
-- the project request is stored. AWS CodeStar retrieves the file during
-- project creation.
--
-- /See:/ 'newToolchainSource' smart constructor.
data ToolchainSource = ToolchainSource'
  { -- | The Amazon S3 bucket where the toolchain template file provided with the
    -- project request is stored.
    s3 :: S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ToolchainSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'toolchainSource_s3' - The Amazon S3 bucket where the toolchain template file provided with the
-- project request is stored.
newToolchainSource ::
  -- | 's3'
  S3Location ->
  ToolchainSource
newToolchainSource pS3_ = ToolchainSource' {s3 = pS3_}

-- | The Amazon S3 bucket where the toolchain template file provided with the
-- project request is stored.
toolchainSource_s3 :: Lens.Lens' ToolchainSource S3Location
toolchainSource_s3 = Lens.lens (\ToolchainSource' {s3} -> s3) (\s@ToolchainSource' {} a -> s {s3 = a} :: ToolchainSource)

instance Prelude.Hashable ToolchainSource where
  hashWithSalt _salt ToolchainSource' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData ToolchainSource where
  rnf ToolchainSource' {..} = Prelude.rnf s3

instance Core.ToJSON ToolchainSource where
  toJSON ToolchainSource' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("s3" Core..= s3)])

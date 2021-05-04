{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeStar.Types.ToolchainSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.ToolchainSource where

import Network.AWS.CodeStar.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable ToolchainSource

instance Prelude.NFData ToolchainSource

instance Prelude.ToJSON ToolchainSource where
  toJSON ToolchainSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("s3" Prelude..= s3)]
      )

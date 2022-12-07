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
-- Module      : Amazonka.CodeStar.Types.CodeSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.CodeSource where

import Amazonka.CodeStar.Types.S3Location
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location where the source code files provided with the project
-- request are stored. AWS CodeStar retrieves the files during project
-- creation.
--
-- /See:/ 'newCodeSource' smart constructor.
data CodeSource = CodeSource'
  { -- | Information about the Amazon S3 location where the source code files
    -- provided with the project request are stored.
    s3 :: S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'codeSource_s3' - Information about the Amazon S3 location where the source code files
-- provided with the project request are stored.
newCodeSource ::
  -- | 's3'
  S3Location ->
  CodeSource
newCodeSource pS3_ = CodeSource' {s3 = pS3_}

-- | Information about the Amazon S3 location where the source code files
-- provided with the project request are stored.
codeSource_s3 :: Lens.Lens' CodeSource S3Location
codeSource_s3 = Lens.lens (\CodeSource' {s3} -> s3) (\s@CodeSource' {} a -> s {s3 = a} :: CodeSource)

instance Prelude.Hashable CodeSource where
  hashWithSalt _salt CodeSource' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData CodeSource where
  rnf CodeSource' {..} = Prelude.rnf s3

instance Data.ToJSON CodeSource where
  toJSON CodeSource' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("s3" Data..= s3)])

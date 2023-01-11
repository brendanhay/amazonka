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
-- Module      : Amazonka.Lambda.Types.AccountLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.AccountLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Limits that are related to concurrency and storage. All file and storage
-- sizes are in bytes.
--
-- /See:/ 'newAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { -- | The maximum size of a function\'s deployment package and layers when
    -- they\'re extracted.
    codeSizeUnzipped :: Prelude.Maybe Prelude.Integer,
    -- | The maximum size of a deployment package when it\'s uploaded directly to
    -- Lambda. Use Amazon S3 for larger files.
    codeSizeZipped :: Prelude.Maybe Prelude.Integer,
    -- | The maximum number of simultaneous function executions.
    concurrentExecutions :: Prelude.Maybe Prelude.Int,
    -- | The amount of storage space that you can use for all deployment packages
    -- and layer archives.
    totalCodeSize :: Prelude.Maybe Prelude.Integer,
    -- | The maximum number of simultaneous function executions, minus the
    -- capacity that\'s reserved for individual functions with
    -- PutFunctionConcurrency.
    unreservedConcurrentExecutions :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSizeUnzipped', 'accountLimit_codeSizeUnzipped' - The maximum size of a function\'s deployment package and layers when
-- they\'re extracted.
--
-- 'codeSizeZipped', 'accountLimit_codeSizeZipped' - The maximum size of a deployment package when it\'s uploaded directly to
-- Lambda. Use Amazon S3 for larger files.
--
-- 'concurrentExecutions', 'accountLimit_concurrentExecutions' - The maximum number of simultaneous function executions.
--
-- 'totalCodeSize', 'accountLimit_totalCodeSize' - The amount of storage space that you can use for all deployment packages
-- and layer archives.
--
-- 'unreservedConcurrentExecutions', 'accountLimit_unreservedConcurrentExecutions' - The maximum number of simultaneous function executions, minus the
-- capacity that\'s reserved for individual functions with
-- PutFunctionConcurrency.
newAccountLimit ::
  AccountLimit
newAccountLimit =
  AccountLimit'
    { codeSizeUnzipped = Prelude.Nothing,
      codeSizeZipped = Prelude.Nothing,
      concurrentExecutions = Prelude.Nothing,
      totalCodeSize = Prelude.Nothing,
      unreservedConcurrentExecutions = Prelude.Nothing
    }

-- | The maximum size of a function\'s deployment package and layers when
-- they\'re extracted.
accountLimit_codeSizeUnzipped :: Lens.Lens' AccountLimit (Prelude.Maybe Prelude.Integer)
accountLimit_codeSizeUnzipped = Lens.lens (\AccountLimit' {codeSizeUnzipped} -> codeSizeUnzipped) (\s@AccountLimit' {} a -> s {codeSizeUnzipped = a} :: AccountLimit)

-- | The maximum size of a deployment package when it\'s uploaded directly to
-- Lambda. Use Amazon S3 for larger files.
accountLimit_codeSizeZipped :: Lens.Lens' AccountLimit (Prelude.Maybe Prelude.Integer)
accountLimit_codeSizeZipped = Lens.lens (\AccountLimit' {codeSizeZipped} -> codeSizeZipped) (\s@AccountLimit' {} a -> s {codeSizeZipped = a} :: AccountLimit)

-- | The maximum number of simultaneous function executions.
accountLimit_concurrentExecutions :: Lens.Lens' AccountLimit (Prelude.Maybe Prelude.Int)
accountLimit_concurrentExecutions = Lens.lens (\AccountLimit' {concurrentExecutions} -> concurrentExecutions) (\s@AccountLimit' {} a -> s {concurrentExecutions = a} :: AccountLimit)

-- | The amount of storage space that you can use for all deployment packages
-- and layer archives.
accountLimit_totalCodeSize :: Lens.Lens' AccountLimit (Prelude.Maybe Prelude.Integer)
accountLimit_totalCodeSize = Lens.lens (\AccountLimit' {totalCodeSize} -> totalCodeSize) (\s@AccountLimit' {} a -> s {totalCodeSize = a} :: AccountLimit)

-- | The maximum number of simultaneous function executions, minus the
-- capacity that\'s reserved for individual functions with
-- PutFunctionConcurrency.
accountLimit_unreservedConcurrentExecutions :: Lens.Lens' AccountLimit (Prelude.Maybe Prelude.Natural)
accountLimit_unreservedConcurrentExecutions = Lens.lens (\AccountLimit' {unreservedConcurrentExecutions} -> unreservedConcurrentExecutions) (\s@AccountLimit' {} a -> s {unreservedConcurrentExecutions = a} :: AccountLimit)

instance Data.FromJSON AccountLimit where
  parseJSON =
    Data.withObject
      "AccountLimit"
      ( \x ->
          AccountLimit'
            Prelude.<$> (x Data..:? "CodeSizeUnzipped")
            Prelude.<*> (x Data..:? "CodeSizeZipped")
            Prelude.<*> (x Data..:? "ConcurrentExecutions")
            Prelude.<*> (x Data..:? "TotalCodeSize")
            Prelude.<*> (x Data..:? "UnreservedConcurrentExecutions")
      )

instance Prelude.Hashable AccountLimit where
  hashWithSalt _salt AccountLimit' {..} =
    _salt `Prelude.hashWithSalt` codeSizeUnzipped
      `Prelude.hashWithSalt` codeSizeZipped
      `Prelude.hashWithSalt` concurrentExecutions
      `Prelude.hashWithSalt` totalCodeSize
      `Prelude.hashWithSalt` unreservedConcurrentExecutions

instance Prelude.NFData AccountLimit where
  rnf AccountLimit' {..} =
    Prelude.rnf codeSizeUnzipped
      `Prelude.seq` Prelude.rnf codeSizeZipped
      `Prelude.seq` Prelude.rnf concurrentExecutions
      `Prelude.seq` Prelude.rnf totalCodeSize
      `Prelude.seq` Prelude.rnf unreservedConcurrentExecutions

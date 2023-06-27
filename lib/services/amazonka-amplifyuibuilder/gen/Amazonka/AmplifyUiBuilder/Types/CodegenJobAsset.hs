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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenJobAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenJobAsset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an asset for a code generation job.
--
-- /See:/ 'newCodegenJobAsset' smart constructor.
data CodegenJobAsset = CodegenJobAsset'
  { -- | The URL to use to access the asset.
    downloadUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenJobAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downloadUrl', 'codegenJobAsset_downloadUrl' - The URL to use to access the asset.
newCodegenJobAsset ::
  CodegenJobAsset
newCodegenJobAsset =
  CodegenJobAsset' {downloadUrl = Prelude.Nothing}

-- | The URL to use to access the asset.
codegenJobAsset_downloadUrl :: Lens.Lens' CodegenJobAsset (Prelude.Maybe Prelude.Text)
codegenJobAsset_downloadUrl = Lens.lens (\CodegenJobAsset' {downloadUrl} -> downloadUrl) (\s@CodegenJobAsset' {} a -> s {downloadUrl = a} :: CodegenJobAsset)

instance Data.FromJSON CodegenJobAsset where
  parseJSON =
    Data.withObject
      "CodegenJobAsset"
      ( \x ->
          CodegenJobAsset'
            Prelude.<$> (x Data..:? "downloadUrl")
      )

instance Prelude.Hashable CodegenJobAsset where
  hashWithSalt _salt CodegenJobAsset' {..} =
    _salt `Prelude.hashWithSalt` downloadUrl

instance Prelude.NFData CodegenJobAsset where
  rnf CodegenJobAsset' {..} = Prelude.rnf downloadUrl

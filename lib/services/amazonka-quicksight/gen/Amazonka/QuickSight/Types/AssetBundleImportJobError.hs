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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an error that occurred within an Asset Bundle import
-- execution.
--
-- /See:/ 'newAssetBundleImportJobError' smart constructor.
data AssetBundleImportJobError = AssetBundleImportJobError'
  { -- | The ARN of the resource whose processing caused an error.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The specific error type or the error that occurred.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleImportJobError_arn' - The ARN of the resource whose processing caused an error.
--
-- 'message', 'assetBundleImportJobError_message' - A description of the error.
--
-- 'type'', 'assetBundleImportJobError_type' - The specific error type or the error that occurred.
newAssetBundleImportJobError ::
  AssetBundleImportJobError
newAssetBundleImportJobError =
  AssetBundleImportJobError'
    { arn = Prelude.Nothing,
      message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN of the resource whose processing caused an error.
assetBundleImportJobError_arn :: Lens.Lens' AssetBundleImportJobError (Prelude.Maybe Prelude.Text)
assetBundleImportJobError_arn = Lens.lens (\AssetBundleImportJobError' {arn} -> arn) (\s@AssetBundleImportJobError' {} a -> s {arn = a} :: AssetBundleImportJobError)

-- | A description of the error.
assetBundleImportJobError_message :: Lens.Lens' AssetBundleImportJobError (Prelude.Maybe Prelude.Text)
assetBundleImportJobError_message = Lens.lens (\AssetBundleImportJobError' {message} -> message) (\s@AssetBundleImportJobError' {} a -> s {message = a} :: AssetBundleImportJobError)

-- | The specific error type or the error that occurred.
assetBundleImportJobError_type :: Lens.Lens' AssetBundleImportJobError (Prelude.Maybe Prelude.Text)
assetBundleImportJobError_type = Lens.lens (\AssetBundleImportJobError' {type'} -> type') (\s@AssetBundleImportJobError' {} a -> s {type' = a} :: AssetBundleImportJobError)

instance Data.FromJSON AssetBundleImportJobError where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobError"
      ( \x ->
          AssetBundleImportJobError'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable AssetBundleImportJobError where
  hashWithSalt _salt AssetBundleImportJobError' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AssetBundleImportJobError where
  rnf AssetBundleImportJobError' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf type'

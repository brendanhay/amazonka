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
-- Module      : Amazonka.QuickSight.Types.AssetBundleExportJobError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleExportJobError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an error that occurred during an Asset Bundle export job.
--
-- /See:/ 'newAssetBundleExportJobError' smart constructor.
data AssetBundleExportJobError = AssetBundleExportJobError'
  { -- | The ARN of the resource whose processing caused an error.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The specific error type of the error that occurred.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleExportJobError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'assetBundleExportJobError_arn' - The ARN of the resource whose processing caused an error.
--
-- 'message', 'assetBundleExportJobError_message' - A description of the error.
--
-- 'type'', 'assetBundleExportJobError_type' - The specific error type of the error that occurred.
newAssetBundleExportJobError ::
  AssetBundleExportJobError
newAssetBundleExportJobError =
  AssetBundleExportJobError'
    { arn = Prelude.Nothing,
      message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN of the resource whose processing caused an error.
assetBundleExportJobError_arn :: Lens.Lens' AssetBundleExportJobError (Prelude.Maybe Prelude.Text)
assetBundleExportJobError_arn = Lens.lens (\AssetBundleExportJobError' {arn} -> arn) (\s@AssetBundleExportJobError' {} a -> s {arn = a} :: AssetBundleExportJobError)

-- | A description of the error.
assetBundleExportJobError_message :: Lens.Lens' AssetBundleExportJobError (Prelude.Maybe Prelude.Text)
assetBundleExportJobError_message = Lens.lens (\AssetBundleExportJobError' {message} -> message) (\s@AssetBundleExportJobError' {} a -> s {message = a} :: AssetBundleExportJobError)

-- | The specific error type of the error that occurred.
assetBundleExportJobError_type :: Lens.Lens' AssetBundleExportJobError (Prelude.Maybe Prelude.Text)
assetBundleExportJobError_type = Lens.lens (\AssetBundleExportJobError' {type'} -> type') (\s@AssetBundleExportJobError' {} a -> s {type' = a} :: AssetBundleExportJobError)

instance Data.FromJSON AssetBundleExportJobError where
  parseJSON =
    Data.withObject
      "AssetBundleExportJobError"
      ( \x ->
          AssetBundleExportJobError'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable AssetBundleExportJobError where
  hashWithSalt _salt AssetBundleExportJobError' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AssetBundleExportJobError where
  rnf AssetBundleExportJobError' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf type'

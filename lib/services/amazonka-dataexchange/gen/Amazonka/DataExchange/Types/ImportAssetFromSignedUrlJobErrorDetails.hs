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
-- Module      : Amazonka.DataExchange.Types.ImportAssetFromSignedUrlJobErrorDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetFromSignedUrlJobErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the job error.
--
-- /See:/ 'newImportAssetFromSignedUrlJobErrorDetails' smart constructor.
data ImportAssetFromSignedUrlJobErrorDetails = ImportAssetFromSignedUrlJobErrorDetails'
  { -- | Details about the job error.
    assetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetFromSignedUrlJobErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetName', 'importAssetFromSignedUrlJobErrorDetails_assetName' - Details about the job error.
newImportAssetFromSignedUrlJobErrorDetails ::
  -- | 'assetName'
  Prelude.Text ->
  ImportAssetFromSignedUrlJobErrorDetails
newImportAssetFromSignedUrlJobErrorDetails
  pAssetName_ =
    ImportAssetFromSignedUrlJobErrorDetails'
      { assetName =
          pAssetName_
      }

-- | Details about the job error.
importAssetFromSignedUrlJobErrorDetails_assetName :: Lens.Lens' ImportAssetFromSignedUrlJobErrorDetails Prelude.Text
importAssetFromSignedUrlJobErrorDetails_assetName = Lens.lens (\ImportAssetFromSignedUrlJobErrorDetails' {assetName} -> assetName) (\s@ImportAssetFromSignedUrlJobErrorDetails' {} a -> s {assetName = a} :: ImportAssetFromSignedUrlJobErrorDetails)

instance
  Data.FromJSON
    ImportAssetFromSignedUrlJobErrorDetails
  where
  parseJSON =
    Data.withObject
      "ImportAssetFromSignedUrlJobErrorDetails"
      ( \x ->
          ImportAssetFromSignedUrlJobErrorDetails'
            Prelude.<$> (x Data..: "AssetName")
      )

instance
  Prelude.Hashable
    ImportAssetFromSignedUrlJobErrorDetails
  where
  hashWithSalt
    _salt
    ImportAssetFromSignedUrlJobErrorDetails' {..} =
      _salt `Prelude.hashWithSalt` assetName

instance
  Prelude.NFData
    ImportAssetFromSignedUrlJobErrorDetails
  where
  rnf ImportAssetFromSignedUrlJobErrorDetails' {..} =
    Prelude.rnf assetName

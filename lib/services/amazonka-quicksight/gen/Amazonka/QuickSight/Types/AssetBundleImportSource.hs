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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The source of the asset bundle zip file that contains the data that you
-- want to import.
--
-- /See:/ 'newAssetBundleImportSource' smart constructor.
data AssetBundleImportSource = AssetBundleImportSource'
  { -- | The bytes of the base64 encoded asset bundle import zip file. This file
    -- can\'t exceed 20 MB.
    --
    -- If you are calling the API operations from the Amazon Web Services SDK
    -- for Java, JavaScript, Python, or PHP, the SDK encodes base64
    -- automatically to allow the direct setting of the zip file\'s bytes. If
    -- you are using an SDK for a different language or receiving related
    -- errors, try to base64 encode your data.
    body :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | The Amazon S3 URI for an asset bundle import file that exists in an
    -- Amazon S3 bucket that the caller has read access to. The file must be a
    -- zip format file and can\'t exceed 20 MB.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'assetBundleImportSource_body' - The bytes of the base64 encoded asset bundle import zip file. This file
-- can\'t exceed 20 MB.
--
-- If you are calling the API operations from the Amazon Web Services SDK
-- for Java, JavaScript, Python, or PHP, the SDK encodes base64
-- automatically to allow the direct setting of the zip file\'s bytes. If
-- you are using an SDK for a different language or receiving related
-- errors, try to base64 encode your data.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 's3Uri', 'assetBundleImportSource_s3Uri' - The Amazon S3 URI for an asset bundle import file that exists in an
-- Amazon S3 bucket that the caller has read access to. The file must be a
-- zip format file and can\'t exceed 20 MB.
newAssetBundleImportSource ::
  AssetBundleImportSource
newAssetBundleImportSource =
  AssetBundleImportSource'
    { body = Prelude.Nothing,
      s3Uri = Prelude.Nothing
    }

-- | The bytes of the base64 encoded asset bundle import zip file. This file
-- can\'t exceed 20 MB.
--
-- If you are calling the API operations from the Amazon Web Services SDK
-- for Java, JavaScript, Python, or PHP, the SDK encodes base64
-- automatically to allow the direct setting of the zip file\'s bytes. If
-- you are using an SDK for a different language or receiving related
-- errors, try to base64 encode your data.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
assetBundleImportSource_body :: Lens.Lens' AssetBundleImportSource (Prelude.Maybe Prelude.ByteString)
assetBundleImportSource_body = Lens.lens (\AssetBundleImportSource' {body} -> body) (\s@AssetBundleImportSource' {} a -> s {body = a} :: AssetBundleImportSource) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | The Amazon S3 URI for an asset bundle import file that exists in an
-- Amazon S3 bucket that the caller has read access to. The file must be a
-- zip format file and can\'t exceed 20 MB.
assetBundleImportSource_s3Uri :: Lens.Lens' AssetBundleImportSource (Prelude.Maybe Prelude.Text)
assetBundleImportSource_s3Uri = Lens.lens (\AssetBundleImportSource' {s3Uri} -> s3Uri) (\s@AssetBundleImportSource' {} a -> s {s3Uri = a} :: AssetBundleImportSource)

instance Prelude.Hashable AssetBundleImportSource where
  hashWithSalt _salt AssetBundleImportSource' {..} =
    _salt
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData AssetBundleImportSource where
  rnf AssetBundleImportSource' {..} =
    Prelude.rnf body `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON AssetBundleImportSource where
  toJSON AssetBundleImportSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Body" Data..=) Prelude.<$> body,
            ("S3Uri" Data..=) Prelude.<$> s3Uri
          ]
      )

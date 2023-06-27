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
-- Module      : Amazonka.SecurityHub.Types.AwsS3ObjectDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3ObjectDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon S3 object.
--
-- /See:/ 'newAwsS3ObjectDetails' smart constructor.
data AwsS3ObjectDetails = AwsS3ObjectDetails'
  { -- | A standard MIME type describing the format of the object data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The opaque identifier assigned by a web server to a specific version of
    -- a resource found at a URL.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the object was last modified.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the KMS symmetric customer managed key that was used
    -- for the object.
    sSEKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | If the object is stored using server-side encryption, the value of the
    -- server-side encryption algorithm used when storing this object in Amazon
    -- S3.
    serverSideEncryption :: Prelude.Maybe Prelude.Text,
    -- | The version of the object.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3ObjectDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'awsS3ObjectDetails_contentType' - A standard MIME type describing the format of the object data.
--
-- 'eTag', 'awsS3ObjectDetails_eTag' - The opaque identifier assigned by a web server to a specific version of
-- a resource found at a URL.
--
-- 'lastModified', 'awsS3ObjectDetails_lastModified' - Indicates when the object was last modified.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
--
-- 'sSEKMSKeyId', 'awsS3ObjectDetails_sSEKMSKeyId' - The identifier of the KMS symmetric customer managed key that was used
-- for the object.
--
-- 'serverSideEncryption', 'awsS3ObjectDetails_serverSideEncryption' - If the object is stored using server-side encryption, the value of the
-- server-side encryption algorithm used when storing this object in Amazon
-- S3.
--
-- 'versionId', 'awsS3ObjectDetails_versionId' - The version of the object.
newAwsS3ObjectDetails ::
  AwsS3ObjectDetails
newAwsS3ObjectDetails =
  AwsS3ObjectDetails'
    { contentType = Prelude.Nothing,
      eTag = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | A standard MIME type describing the format of the object data.
awsS3ObjectDetails_contentType :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_contentType = Lens.lens (\AwsS3ObjectDetails' {contentType} -> contentType) (\s@AwsS3ObjectDetails' {} a -> s {contentType = a} :: AwsS3ObjectDetails)

-- | The opaque identifier assigned by a web server to a specific version of
-- a resource found at a URL.
awsS3ObjectDetails_eTag :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_eTag = Lens.lens (\AwsS3ObjectDetails' {eTag} -> eTag) (\s@AwsS3ObjectDetails' {} a -> s {eTag = a} :: AwsS3ObjectDetails)

-- | Indicates when the object was last modified.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
awsS3ObjectDetails_lastModified :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_lastModified = Lens.lens (\AwsS3ObjectDetails' {lastModified} -> lastModified) (\s@AwsS3ObjectDetails' {} a -> s {lastModified = a} :: AwsS3ObjectDetails)

-- | The identifier of the KMS symmetric customer managed key that was used
-- for the object.
awsS3ObjectDetails_sSEKMSKeyId :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_sSEKMSKeyId = Lens.lens (\AwsS3ObjectDetails' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@AwsS3ObjectDetails' {} a -> s {sSEKMSKeyId = a} :: AwsS3ObjectDetails)

-- | If the object is stored using server-side encryption, the value of the
-- server-side encryption algorithm used when storing this object in Amazon
-- S3.
awsS3ObjectDetails_serverSideEncryption :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_serverSideEncryption = Lens.lens (\AwsS3ObjectDetails' {serverSideEncryption} -> serverSideEncryption) (\s@AwsS3ObjectDetails' {} a -> s {serverSideEncryption = a} :: AwsS3ObjectDetails)

-- | The version of the object.
awsS3ObjectDetails_versionId :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_versionId = Lens.lens (\AwsS3ObjectDetails' {versionId} -> versionId) (\s@AwsS3ObjectDetails' {} a -> s {versionId = a} :: AwsS3ObjectDetails)

instance Data.FromJSON AwsS3ObjectDetails where
  parseJSON =
    Data.withObject
      "AwsS3ObjectDetails"
      ( \x ->
          AwsS3ObjectDetails'
            Prelude.<$> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "ETag")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "SSEKMSKeyId")
            Prelude.<*> (x Data..:? "ServerSideEncryption")
            Prelude.<*> (x Data..:? "VersionId")
      )

instance Prelude.Hashable AwsS3ObjectDetails where
  hashWithSalt _salt AwsS3ObjectDetails' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` sSEKMSKeyId
      `Prelude.hashWithSalt` serverSideEncryption
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData AwsS3ObjectDetails where
  rnf AwsS3ObjectDetails' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf sSEKMSKeyId
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToJSON AwsS3ObjectDetails where
  toJSON AwsS3ObjectDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentType" Data..=) Prelude.<$> contentType,
            ("ETag" Data..=) Prelude.<$> eTag,
            ("LastModified" Data..=) Prelude.<$> lastModified,
            ("SSEKMSKeyId" Data..=) Prelude.<$> sSEKMSKeyId,
            ("ServerSideEncryption" Data..=)
              Prelude.<$> serverSideEncryption,
            ("VersionId" Data..=) Prelude.<$> versionId
          ]
      )

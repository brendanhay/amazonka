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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3ObjectDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon S3 object.
--
-- /See:/ 'newAwsS3ObjectDetails' smart constructor.
data AwsS3ObjectDetails = AwsS3ObjectDetails'
  { -- | The opaque identifier assigned by a web server to a specific version of
    -- a resource found at a URL.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The version of the object.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the KMS symmetric customer managed key that was used
    -- for the object.
    sSEKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the object was last modified.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | If the object is stored using server-side encryption, the value of the
    -- server-side encryption algorithm used when storing this object in Amazon
    -- S3.
    serverSideEncryption :: Prelude.Maybe Prelude.Text,
    -- | A standard MIME type describing the format of the object data.
    contentType :: Prelude.Maybe Prelude.Text
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
-- 'eTag', 'awsS3ObjectDetails_eTag' - The opaque identifier assigned by a web server to a specific version of
-- a resource found at a URL.
--
-- 'versionId', 'awsS3ObjectDetails_versionId' - The version of the object.
--
-- 'sSEKMSKeyId', 'awsS3ObjectDetails_sSEKMSKeyId' - The identifier of the KMS symmetric customer managed key that was used
-- for the object.
--
-- 'lastModified', 'awsS3ObjectDetails_lastModified' - Indicates when the object was last modified.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'serverSideEncryption', 'awsS3ObjectDetails_serverSideEncryption' - If the object is stored using server-side encryption, the value of the
-- server-side encryption algorithm used when storing this object in Amazon
-- S3.
--
-- 'contentType', 'awsS3ObjectDetails_contentType' - A standard MIME type describing the format of the object data.
newAwsS3ObjectDetails ::
  AwsS3ObjectDetails
newAwsS3ObjectDetails =
  AwsS3ObjectDetails'
    { eTag = Prelude.Nothing,
      versionId = Prelude.Nothing,
      sSEKMSKeyId = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      contentType = Prelude.Nothing
    }

-- | The opaque identifier assigned by a web server to a specific version of
-- a resource found at a URL.
awsS3ObjectDetails_eTag :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_eTag = Lens.lens (\AwsS3ObjectDetails' {eTag} -> eTag) (\s@AwsS3ObjectDetails' {} a -> s {eTag = a} :: AwsS3ObjectDetails)

-- | The version of the object.
awsS3ObjectDetails_versionId :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_versionId = Lens.lens (\AwsS3ObjectDetails' {versionId} -> versionId) (\s@AwsS3ObjectDetails' {} a -> s {versionId = a} :: AwsS3ObjectDetails)

-- | The identifier of the KMS symmetric customer managed key that was used
-- for the object.
awsS3ObjectDetails_sSEKMSKeyId :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_sSEKMSKeyId = Lens.lens (\AwsS3ObjectDetails' {sSEKMSKeyId} -> sSEKMSKeyId) (\s@AwsS3ObjectDetails' {} a -> s {sSEKMSKeyId = a} :: AwsS3ObjectDetails)

-- | Indicates when the object was last modified.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsS3ObjectDetails_lastModified :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_lastModified = Lens.lens (\AwsS3ObjectDetails' {lastModified} -> lastModified) (\s@AwsS3ObjectDetails' {} a -> s {lastModified = a} :: AwsS3ObjectDetails)

-- | If the object is stored using server-side encryption, the value of the
-- server-side encryption algorithm used when storing this object in Amazon
-- S3.
awsS3ObjectDetails_serverSideEncryption :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_serverSideEncryption = Lens.lens (\AwsS3ObjectDetails' {serverSideEncryption} -> serverSideEncryption) (\s@AwsS3ObjectDetails' {} a -> s {serverSideEncryption = a} :: AwsS3ObjectDetails)

-- | A standard MIME type describing the format of the object data.
awsS3ObjectDetails_contentType :: Lens.Lens' AwsS3ObjectDetails (Prelude.Maybe Prelude.Text)
awsS3ObjectDetails_contentType = Lens.lens (\AwsS3ObjectDetails' {contentType} -> contentType) (\s@AwsS3ObjectDetails' {} a -> s {contentType = a} :: AwsS3ObjectDetails)

instance Core.FromJSON AwsS3ObjectDetails where
  parseJSON =
    Core.withObject
      "AwsS3ObjectDetails"
      ( \x ->
          AwsS3ObjectDetails'
            Prelude.<$> (x Core..:? "ETag")
            Prelude.<*> (x Core..:? "VersionId")
            Prelude.<*> (x Core..:? "SSEKMSKeyId")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "ServerSideEncryption")
            Prelude.<*> (x Core..:? "ContentType")
      )

instance Prelude.Hashable AwsS3ObjectDetails

instance Prelude.NFData AwsS3ObjectDetails

instance Core.ToJSON AwsS3ObjectDetails where
  toJSON AwsS3ObjectDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ETag" Core..=) Prelude.<$> eTag,
            ("VersionId" Core..=) Prelude.<$> versionId,
            ("SSEKMSKeyId" Core..=) Prelude.<$> sSEKMSKeyId,
            ("LastModified" Core..=) Prelude.<$> lastModified,
            ("ServerSideEncryption" Core..=)
              Prelude.<$> serverSideEncryption,
            ("ContentType" Core..=) Prelude.<$> contentType
          ]
      )

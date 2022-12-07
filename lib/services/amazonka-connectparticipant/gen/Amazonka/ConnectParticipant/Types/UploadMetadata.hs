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
-- Module      : Amazonka.ConnectParticipant.Types.UploadMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.UploadMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Fields to be used while uploading the attachment.
--
-- /See:/ 'newUploadMetadata' smart constructor.
data UploadMetadata = UploadMetadata'
  { -- | This is the pre-signed URL that can be used for uploading the file to
    -- Amazon S3 when used in response to
    -- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_StartAttachmentUpload.html StartAttachmentUpload>.
    url :: Prelude.Maybe Prelude.Text,
    -- | The headers to be provided while uploading the file to the URL.
    headersToInclude :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The expiration time of the URL in ISO timestamp. It\'s specified in ISO
    -- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
    -- 2019-11-08T02:41:28.172Z.
    urlExpiry :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'uploadMetadata_url' - This is the pre-signed URL that can be used for uploading the file to
-- Amazon S3 when used in response to
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_StartAttachmentUpload.html StartAttachmentUpload>.
--
-- 'headersToInclude', 'uploadMetadata_headersToInclude' - The headers to be provided while uploading the file to the URL.
--
-- 'urlExpiry', 'uploadMetadata_urlExpiry' - The expiration time of the URL in ISO timestamp. It\'s specified in ISO
-- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
-- 2019-11-08T02:41:28.172Z.
newUploadMetadata ::
  UploadMetadata
newUploadMetadata =
  UploadMetadata'
    { url = Prelude.Nothing,
      headersToInclude = Prelude.Nothing,
      urlExpiry = Prelude.Nothing
    }

-- | This is the pre-signed URL that can be used for uploading the file to
-- Amazon S3 when used in response to
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_StartAttachmentUpload.html StartAttachmentUpload>.
uploadMetadata_url :: Lens.Lens' UploadMetadata (Prelude.Maybe Prelude.Text)
uploadMetadata_url = Lens.lens (\UploadMetadata' {url} -> url) (\s@UploadMetadata' {} a -> s {url = a} :: UploadMetadata)

-- | The headers to be provided while uploading the file to the URL.
uploadMetadata_headersToInclude :: Lens.Lens' UploadMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
uploadMetadata_headersToInclude = Lens.lens (\UploadMetadata' {headersToInclude} -> headersToInclude) (\s@UploadMetadata' {} a -> s {headersToInclude = a} :: UploadMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The expiration time of the URL in ISO timestamp. It\'s specified in ISO
-- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
-- 2019-11-08T02:41:28.172Z.
uploadMetadata_urlExpiry :: Lens.Lens' UploadMetadata (Prelude.Maybe Prelude.Text)
uploadMetadata_urlExpiry = Lens.lens (\UploadMetadata' {urlExpiry} -> urlExpiry) (\s@UploadMetadata' {} a -> s {urlExpiry = a} :: UploadMetadata)

instance Data.FromJSON UploadMetadata where
  parseJSON =
    Data.withObject
      "UploadMetadata"
      ( \x ->
          UploadMetadata'
            Prelude.<$> (x Data..:? "Url")
            Prelude.<*> ( x Data..:? "HeadersToInclude"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UrlExpiry")
      )

instance Prelude.Hashable UploadMetadata where
  hashWithSalt _salt UploadMetadata' {..} =
    _salt `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` headersToInclude
      `Prelude.hashWithSalt` urlExpiry

instance Prelude.NFData UploadMetadata where
  rnf UploadMetadata' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf headersToInclude
      `Prelude.seq` Prelude.rnf urlExpiry

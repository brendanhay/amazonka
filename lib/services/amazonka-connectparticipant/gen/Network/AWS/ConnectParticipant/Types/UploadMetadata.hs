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
-- Module      : Network.AWS.ConnectParticipant.Types.UploadMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ConnectParticipant.Types.UploadMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Fields to be used while uploading the attachment.
--
-- /See:/ 'newUploadMetadata' smart constructor.
data UploadMetadata = UploadMetadata'
  { -- | The expiration time of the URL in ISO timestamp. It\'s specified in ISO
    -- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
    -- 2019-11-08T02:41:28.172Z.
    urlExpiry :: Prelude.Maybe Prelude.Text,
    -- | The headers to be provided while uploading the file to the URL.
    headersToInclude :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The pre-signed URL using which file would be downloaded from Amazon S3
    -- by the API caller.
    url :: Prelude.Maybe Prelude.Text
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
-- 'urlExpiry', 'uploadMetadata_urlExpiry' - The expiration time of the URL in ISO timestamp. It\'s specified in ISO
-- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
-- 2019-11-08T02:41:28.172Z.
--
-- 'headersToInclude', 'uploadMetadata_headersToInclude' - The headers to be provided while uploading the file to the URL.
--
-- 'url', 'uploadMetadata_url' - The pre-signed URL using which file would be downloaded from Amazon S3
-- by the API caller.
newUploadMetadata ::
  UploadMetadata
newUploadMetadata =
  UploadMetadata'
    { urlExpiry = Prelude.Nothing,
      headersToInclude = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The expiration time of the URL in ISO timestamp. It\'s specified in ISO
-- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
-- 2019-11-08T02:41:28.172Z.
uploadMetadata_urlExpiry :: Lens.Lens' UploadMetadata (Prelude.Maybe Prelude.Text)
uploadMetadata_urlExpiry = Lens.lens (\UploadMetadata' {urlExpiry} -> urlExpiry) (\s@UploadMetadata' {} a -> s {urlExpiry = a} :: UploadMetadata)

-- | The headers to be provided while uploading the file to the URL.
uploadMetadata_headersToInclude :: Lens.Lens' UploadMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
uploadMetadata_headersToInclude = Lens.lens (\UploadMetadata' {headersToInclude} -> headersToInclude) (\s@UploadMetadata' {} a -> s {headersToInclude = a} :: UploadMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The pre-signed URL using which file would be downloaded from Amazon S3
-- by the API caller.
uploadMetadata_url :: Lens.Lens' UploadMetadata (Prelude.Maybe Prelude.Text)
uploadMetadata_url = Lens.lens (\UploadMetadata' {url} -> url) (\s@UploadMetadata' {} a -> s {url = a} :: UploadMetadata)

instance Core.FromJSON UploadMetadata where
  parseJSON =
    Core.withObject
      "UploadMetadata"
      ( \x ->
          UploadMetadata'
            Prelude.<$> (x Core..:? "UrlExpiry")
            Prelude.<*> ( x Core..:? "HeadersToInclude"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Url")
      )

instance Prelude.Hashable UploadMetadata

instance Prelude.NFData UploadMetadata

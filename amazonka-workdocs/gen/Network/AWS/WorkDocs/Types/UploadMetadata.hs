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
-- Module      : Network.AWS.WorkDocs.Types.UploadMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UploadMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the upload.
--
-- /See:/ 'newUploadMetadata' smart constructor.
data UploadMetadata = UploadMetadata'
  { -- | The signed headers.
    signedHeaders :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The URL of the upload.
    uploadUrl :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UploadMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signedHeaders', 'uploadMetadata_signedHeaders' - The signed headers.
--
-- 'uploadUrl', 'uploadMetadata_uploadUrl' - The URL of the upload.
newUploadMetadata ::
  UploadMetadata
newUploadMetadata =
  UploadMetadata'
    { signedHeaders = Core.Nothing,
      uploadUrl = Core.Nothing
    }

-- | The signed headers.
uploadMetadata_signedHeaders :: Lens.Lens' UploadMetadata (Core.Maybe (Core.HashMap Core.Text Core.Text))
uploadMetadata_signedHeaders = Lens.lens (\UploadMetadata' {signedHeaders} -> signedHeaders) (\s@UploadMetadata' {} a -> s {signedHeaders = a} :: UploadMetadata) Core.. Lens.mapping Lens._Coerce

-- | The URL of the upload.
uploadMetadata_uploadUrl :: Lens.Lens' UploadMetadata (Core.Maybe Core.Text)
uploadMetadata_uploadUrl = Lens.lens (\UploadMetadata' {uploadUrl} -> uploadUrl) (\s@UploadMetadata' {} a -> s {uploadUrl = a} :: UploadMetadata) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON UploadMetadata where
  parseJSON =
    Core.withObject
      "UploadMetadata"
      ( \x ->
          UploadMetadata'
            Core.<$> (x Core..:? "SignedHeaders" Core..!= Core.mempty)
            Core.<*> (x Core..:? "UploadUrl")
      )

instance Core.Hashable UploadMetadata

instance Core.NFData UploadMetadata

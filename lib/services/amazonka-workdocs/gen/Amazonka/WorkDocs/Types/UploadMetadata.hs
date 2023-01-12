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
-- Module      : Amazonka.WorkDocs.Types.UploadMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.UploadMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the upload.
--
-- /See:/ 'newUploadMetadata' smart constructor.
data UploadMetadata = UploadMetadata'
  { -- | The signed headers.
    signedHeaders :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URL of the upload.
    uploadUrl :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
    { signedHeaders = Prelude.Nothing,
      uploadUrl = Prelude.Nothing
    }

-- | The signed headers.
uploadMetadata_signedHeaders :: Lens.Lens' UploadMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
uploadMetadata_signedHeaders = Lens.lens (\UploadMetadata' {signedHeaders} -> signedHeaders) (\s@UploadMetadata' {} a -> s {signedHeaders = a} :: UploadMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the upload.
uploadMetadata_uploadUrl :: Lens.Lens' UploadMetadata (Prelude.Maybe Prelude.Text)
uploadMetadata_uploadUrl = Lens.lens (\UploadMetadata' {uploadUrl} -> uploadUrl) (\s@UploadMetadata' {} a -> s {uploadUrl = a} :: UploadMetadata) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON UploadMetadata where
  parseJSON =
    Data.withObject
      "UploadMetadata"
      ( \x ->
          UploadMetadata'
            Prelude.<$> (x Data..:? "SignedHeaders" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UploadUrl")
      )

instance Prelude.Hashable UploadMetadata where
  hashWithSalt _salt UploadMetadata' {..} =
    _salt `Prelude.hashWithSalt` signedHeaders
      `Prelude.hashWithSalt` uploadUrl

instance Prelude.NFData UploadMetadata where
  rnf UploadMetadata' {..} =
    Prelude.rnf signedHeaders
      `Prelude.seq` Prelude.rnf uploadUrl

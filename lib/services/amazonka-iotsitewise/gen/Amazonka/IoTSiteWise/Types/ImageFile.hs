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
-- Module      : Amazonka.IoTSiteWise.Types.ImageFile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ImageFile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.ImageFileType
import qualified Amazonka.Prelude as Prelude

-- | Contains an image file.
--
-- /See:/ 'newImageFile' smart constructor.
data ImageFile = ImageFile'
  { -- | The image file contents, represented as a base64-encoded string. The
    -- file size must be less than 1 MB.
    data' :: Data.Base64,
    -- | The file type of the image.
    type' :: ImageFileType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'imageFile_data' - The image file contents, represented as a base64-encoded string. The
-- file size must be less than 1 MB.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'type'', 'imageFile_type' - The file type of the image.
newImageFile ::
  -- | 'data''
  Prelude.ByteString ->
  -- | 'type''
  ImageFileType ->
  ImageFile
newImageFile pData_ pType_ =
  ImageFile'
    { data' = Data._Base64 Lens.# pData_,
      type' = pType_
    }

-- | The image file contents, represented as a base64-encoded string. The
-- file size must be less than 1 MB.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
imageFile_data :: Lens.Lens' ImageFile Prelude.ByteString
imageFile_data = Lens.lens (\ImageFile' {data'} -> data') (\s@ImageFile' {} a -> s {data' = a} :: ImageFile) Prelude.. Data._Base64

-- | The file type of the image.
imageFile_type :: Lens.Lens' ImageFile ImageFileType
imageFile_type = Lens.lens (\ImageFile' {type'} -> type') (\s@ImageFile' {} a -> s {type' = a} :: ImageFile)

instance Prelude.Hashable ImageFile where
  hashWithSalt _salt ImageFile' {..} =
    _salt `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ImageFile where
  rnf ImageFile' {..} =
    Prelude.rnf data' `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ImageFile where
  toJSON ImageFile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("data" Data..= data'),
            Prelude.Just ("type" Data..= type')
          ]
      )

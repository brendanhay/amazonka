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
-- Module      : Amazonka.Textract.Types.DocumentLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.DocumentLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.S3Object

-- | The Amazon S3 bucket that contains the document to be processed. It\'s
-- used by asynchronous operations such as StartDocumentTextDetection.
--
-- The input document can be an image file in JPEG or PNG format. It can
-- also be a file in PDF format.
--
-- /See:/ 'newDocumentLocation' smart constructor.
data DocumentLocation = DocumentLocation'
  { -- | The Amazon S3 bucket that contains the input document.
    s3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Object', 'documentLocation_s3Object' - The Amazon S3 bucket that contains the input document.
newDocumentLocation ::
  DocumentLocation
newDocumentLocation =
  DocumentLocation' {s3Object = Prelude.Nothing}

-- | The Amazon S3 bucket that contains the input document.
documentLocation_s3Object :: Lens.Lens' DocumentLocation (Prelude.Maybe S3Object)
documentLocation_s3Object = Lens.lens (\DocumentLocation' {s3Object} -> s3Object) (\s@DocumentLocation' {} a -> s {s3Object = a} :: DocumentLocation)

instance Prelude.Hashable DocumentLocation where
  hashWithSalt _salt DocumentLocation' {..} =
    _salt `Prelude.hashWithSalt` s3Object

instance Prelude.NFData DocumentLocation where
  rnf DocumentLocation' {..} = Prelude.rnf s3Object

instance Data.ToJSON DocumentLocation where
  toJSON DocumentLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Object" Data..=) Prelude.<$> s3Object]
      )

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
-- Module      : Amazonka.Rekognition.Types.AuditImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.AuditImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.S3Object

-- | An image that is picked from the Face Liveness video and returned for
-- audit trail purposes, returned as Base64-encoded bytes.
--
-- /See:/ 'newAuditImage' smart constructor.
data AuditImage = AuditImage'
  { boundingBox :: Prelude.Maybe BoundingBox,
    -- | The Base64-encoded bytes representing an image selected from the Face
    -- Liveness video and returned for audit purposes.
    bytes :: Prelude.Maybe (Data.Sensitive Data.Base64),
    s3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'auditImage_boundingBox' - Undocumented member.
--
-- 'bytes', 'auditImage_bytes' - The Base64-encoded bytes representing an image selected from the Face
-- Liveness video and returned for audit purposes.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 's3Object', 'auditImage_s3Object' - Undocumented member.
newAuditImage ::
  AuditImage
newAuditImage =
  AuditImage'
    { boundingBox = Prelude.Nothing,
      bytes = Prelude.Nothing,
      s3Object = Prelude.Nothing
    }

-- | Undocumented member.
auditImage_boundingBox :: Lens.Lens' AuditImage (Prelude.Maybe BoundingBox)
auditImage_boundingBox = Lens.lens (\AuditImage' {boundingBox} -> boundingBox) (\s@AuditImage' {} a -> s {boundingBox = a} :: AuditImage)

-- | The Base64-encoded bytes representing an image selected from the Face
-- Liveness video and returned for audit purposes.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
auditImage_bytes :: Lens.Lens' AuditImage (Prelude.Maybe Prelude.ByteString)
auditImage_bytes = Lens.lens (\AuditImage' {bytes} -> bytes) (\s@AuditImage' {} a -> s {bytes = a} :: AuditImage) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | Undocumented member.
auditImage_s3Object :: Lens.Lens' AuditImage (Prelude.Maybe S3Object)
auditImage_s3Object = Lens.lens (\AuditImage' {s3Object} -> s3Object) (\s@AuditImage' {} a -> s {s3Object = a} :: AuditImage)

instance Data.FromJSON AuditImage where
  parseJSON =
    Data.withObject
      "AuditImage"
      ( \x ->
          AuditImage'
            Prelude.<$> (x Data..:? "BoundingBox")
            Prelude.<*> (x Data..:? "Bytes")
            Prelude.<*> (x Data..:? "S3Object")
      )

instance Prelude.Hashable AuditImage where
  hashWithSalt _salt AuditImage' {..} =
    _salt
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` bytes
      `Prelude.hashWithSalt` s3Object

instance Prelude.NFData AuditImage where
  rnf AuditImage' {..} =
    Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf bytes
      `Prelude.seq` Prelude.rnf s3Object

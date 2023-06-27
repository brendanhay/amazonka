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
-- Module      : Amazonka.Textract.Types.SignatureDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.SignatureDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.Geometry

-- | Information regarding a detected signature on a page.
--
-- /See:/ 'newSignatureDetection' smart constructor.
data SignatureDetection = SignatureDetection'
  { -- | The confidence, from 0 to 100, in the predicted values for a detected
    -- signature.
    confidence :: Prelude.Maybe Prelude.Double,
    geometry :: Prelude.Maybe Geometry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignatureDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'signatureDetection_confidence' - The confidence, from 0 to 100, in the predicted values for a detected
-- signature.
--
-- 'geometry', 'signatureDetection_geometry' - Undocumented member.
newSignatureDetection ::
  SignatureDetection
newSignatureDetection =
  SignatureDetection'
    { confidence = Prelude.Nothing,
      geometry = Prelude.Nothing
    }

-- | The confidence, from 0 to 100, in the predicted values for a detected
-- signature.
signatureDetection_confidence :: Lens.Lens' SignatureDetection (Prelude.Maybe Prelude.Double)
signatureDetection_confidence = Lens.lens (\SignatureDetection' {confidence} -> confidence) (\s@SignatureDetection' {} a -> s {confidence = a} :: SignatureDetection)

-- | Undocumented member.
signatureDetection_geometry :: Lens.Lens' SignatureDetection (Prelude.Maybe Geometry)
signatureDetection_geometry = Lens.lens (\SignatureDetection' {geometry} -> geometry) (\s@SignatureDetection' {} a -> s {geometry = a} :: SignatureDetection)

instance Data.FromJSON SignatureDetection where
  parseJSON =
    Data.withObject
      "SignatureDetection"
      ( \x ->
          SignatureDetection'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Geometry")
      )

instance Prelude.Hashable SignatureDetection where
  hashWithSalt _salt SignatureDetection' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` geometry

instance Prelude.NFData SignatureDetection where
  rnf SignatureDetection' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf geometry

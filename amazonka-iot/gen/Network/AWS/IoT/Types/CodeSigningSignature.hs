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
-- Module      : Network.AWS.IoT.Types.CodeSigningSignature
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigningSignature where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the signature for a file.
--
-- /See:/ 'newCodeSigningSignature' smart constructor.
data CodeSigningSignature = CodeSigningSignature'
  { -- | A base64 encoded binary representation of the code signing signature.
    inlineDocument :: Core.Maybe Core.Base64
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeSigningSignature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inlineDocument', 'codeSigningSignature_inlineDocument' - A base64 encoded binary representation of the code signing signature.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newCodeSigningSignature ::
  CodeSigningSignature
newCodeSigningSignature =
  CodeSigningSignature'
    { inlineDocument =
        Core.Nothing
    }

-- | A base64 encoded binary representation of the code signing signature.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
codeSigningSignature_inlineDocument :: Lens.Lens' CodeSigningSignature (Core.Maybe Core.ByteString)
codeSigningSignature_inlineDocument = Lens.lens (\CodeSigningSignature' {inlineDocument} -> inlineDocument) (\s@CodeSigningSignature' {} a -> s {inlineDocument = a} :: CodeSigningSignature) Core.. Lens.mapping Core._Base64

instance Core.FromJSON CodeSigningSignature where
  parseJSON =
    Core.withObject
      "CodeSigningSignature"
      ( \x ->
          CodeSigningSignature'
            Core.<$> (x Core..:? "inlineDocument")
      )

instance Core.Hashable CodeSigningSignature

instance Core.NFData CodeSigningSignature

instance Core.ToJSON CodeSigningSignature where
  toJSON CodeSigningSignature' {..} =
    Core.object
      ( Core.catMaybes
          [("inlineDocument" Core..=) Core.<$> inlineDocument]
      )

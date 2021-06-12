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
-- Module      : Network.AWS.IoT.Types.CodeSigningCertificateChain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigningCertificateChain where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the certificate chain being used when code signing a file.
--
-- /See:/ 'newCodeSigningCertificateChain' smart constructor.
data CodeSigningCertificateChain = CodeSigningCertificateChain'
  { -- | A base64 encoded binary representation of the code signing certificate
    -- chain.
    inlineDocument :: Core.Maybe Core.Text,
    -- | The name of the certificate.
    certificateName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeSigningCertificateChain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inlineDocument', 'codeSigningCertificateChain_inlineDocument' - A base64 encoded binary representation of the code signing certificate
-- chain.
--
-- 'certificateName', 'codeSigningCertificateChain_certificateName' - The name of the certificate.
newCodeSigningCertificateChain ::
  CodeSigningCertificateChain
newCodeSigningCertificateChain =
  CodeSigningCertificateChain'
    { inlineDocument =
        Core.Nothing,
      certificateName = Core.Nothing
    }

-- | A base64 encoded binary representation of the code signing certificate
-- chain.
codeSigningCertificateChain_inlineDocument :: Lens.Lens' CodeSigningCertificateChain (Core.Maybe Core.Text)
codeSigningCertificateChain_inlineDocument = Lens.lens (\CodeSigningCertificateChain' {inlineDocument} -> inlineDocument) (\s@CodeSigningCertificateChain' {} a -> s {inlineDocument = a} :: CodeSigningCertificateChain)

-- | The name of the certificate.
codeSigningCertificateChain_certificateName :: Lens.Lens' CodeSigningCertificateChain (Core.Maybe Core.Text)
codeSigningCertificateChain_certificateName = Lens.lens (\CodeSigningCertificateChain' {certificateName} -> certificateName) (\s@CodeSigningCertificateChain' {} a -> s {certificateName = a} :: CodeSigningCertificateChain)

instance Core.FromJSON CodeSigningCertificateChain where
  parseJSON =
    Core.withObject
      "CodeSigningCertificateChain"
      ( \x ->
          CodeSigningCertificateChain'
            Core.<$> (x Core..:? "inlineDocument")
            Core.<*> (x Core..:? "certificateName")
      )

instance Core.Hashable CodeSigningCertificateChain

instance Core.NFData CodeSigningCertificateChain

instance Core.ToJSON CodeSigningCertificateChain where
  toJSON CodeSigningCertificateChain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("inlineDocument" Core..=) Core.<$> inlineDocument,
            ("certificateName" Core..=)
              Core.<$> certificateName
          ]
      )

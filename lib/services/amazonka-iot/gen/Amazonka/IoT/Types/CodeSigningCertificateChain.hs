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
-- Module      : Amazonka.IoT.Types.CodeSigningCertificateChain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CodeSigningCertificateChain where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the certificate chain being used when code signing a file.
--
-- /See:/ 'newCodeSigningCertificateChain' smart constructor.
data CodeSigningCertificateChain = CodeSigningCertificateChain'
  { -- | The name of the certificate.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | A base64 encoded binary representation of the code signing certificate
    -- chain.
    inlineDocument :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeSigningCertificateChain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateName', 'codeSigningCertificateChain_certificateName' - The name of the certificate.
--
-- 'inlineDocument', 'codeSigningCertificateChain_inlineDocument' - A base64 encoded binary representation of the code signing certificate
-- chain.
newCodeSigningCertificateChain ::
  CodeSigningCertificateChain
newCodeSigningCertificateChain =
  CodeSigningCertificateChain'
    { certificateName =
        Prelude.Nothing,
      inlineDocument = Prelude.Nothing
    }

-- | The name of the certificate.
codeSigningCertificateChain_certificateName :: Lens.Lens' CodeSigningCertificateChain (Prelude.Maybe Prelude.Text)
codeSigningCertificateChain_certificateName = Lens.lens (\CodeSigningCertificateChain' {certificateName} -> certificateName) (\s@CodeSigningCertificateChain' {} a -> s {certificateName = a} :: CodeSigningCertificateChain)

-- | A base64 encoded binary representation of the code signing certificate
-- chain.
codeSigningCertificateChain_inlineDocument :: Lens.Lens' CodeSigningCertificateChain (Prelude.Maybe Prelude.Text)
codeSigningCertificateChain_inlineDocument = Lens.lens (\CodeSigningCertificateChain' {inlineDocument} -> inlineDocument) (\s@CodeSigningCertificateChain' {} a -> s {inlineDocument = a} :: CodeSigningCertificateChain)

instance Data.FromJSON CodeSigningCertificateChain where
  parseJSON =
    Data.withObject
      "CodeSigningCertificateChain"
      ( \x ->
          CodeSigningCertificateChain'
            Prelude.<$> (x Data..:? "certificateName")
            Prelude.<*> (x Data..:? "inlineDocument")
      )

instance Prelude.Hashable CodeSigningCertificateChain where
  hashWithSalt _salt CodeSigningCertificateChain' {..} =
    _salt `Prelude.hashWithSalt` certificateName
      `Prelude.hashWithSalt` inlineDocument

instance Prelude.NFData CodeSigningCertificateChain where
  rnf CodeSigningCertificateChain' {..} =
    Prelude.rnf certificateName
      `Prelude.seq` Prelude.rnf inlineDocument

instance Data.ToJSON CodeSigningCertificateChain where
  toJSON CodeSigningCertificateChain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateName" Data..=)
              Prelude.<$> certificateName,
            ("inlineDocument" Data..=)
              Prelude.<$> inlineDocument
          ]
      )

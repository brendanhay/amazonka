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
-- Module      : Amazonka.CertificateManagerPCA.Types.KeyUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.KeyUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines one or more purposes for which the key contained in the
-- certificate can be used. Default value for each option is false.
--
-- /See:/ 'newKeyUsage' smart constructor.
data KeyUsage = KeyUsage'
  { -- | Key can be used to sign CRLs.
    cRLSign :: Prelude.Maybe Prelude.Bool,
    -- | Key can be used to decipher data.
    dataEncipherment :: Prelude.Maybe Prelude.Bool,
    -- | Key can be used only to decipher data.
    decipherOnly :: Prelude.Maybe Prelude.Bool,
    -- | Key can be used for digital signing.
    digitalSignature :: Prelude.Maybe Prelude.Bool,
    -- | Key can be used only to encipher data.
    encipherOnly :: Prelude.Maybe Prelude.Bool,
    -- | Key can be used in a key-agreement protocol.
    keyAgreement :: Prelude.Maybe Prelude.Bool,
    -- | Key can be used to sign certificates.
    keyCertSign :: Prelude.Maybe Prelude.Bool,
    -- | Key can be used to encipher data.
    keyEncipherment :: Prelude.Maybe Prelude.Bool,
    -- | Key can be used for non-repudiation.
    nonRepudiation :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cRLSign', 'keyUsage_cRLSign' - Key can be used to sign CRLs.
--
-- 'dataEncipherment', 'keyUsage_dataEncipherment' - Key can be used to decipher data.
--
-- 'decipherOnly', 'keyUsage_decipherOnly' - Key can be used only to decipher data.
--
-- 'digitalSignature', 'keyUsage_digitalSignature' - Key can be used for digital signing.
--
-- 'encipherOnly', 'keyUsage_encipherOnly' - Key can be used only to encipher data.
--
-- 'keyAgreement', 'keyUsage_keyAgreement' - Key can be used in a key-agreement protocol.
--
-- 'keyCertSign', 'keyUsage_keyCertSign' - Key can be used to sign certificates.
--
-- 'keyEncipherment', 'keyUsage_keyEncipherment' - Key can be used to encipher data.
--
-- 'nonRepudiation', 'keyUsage_nonRepudiation' - Key can be used for non-repudiation.
newKeyUsage ::
  KeyUsage
newKeyUsage =
  KeyUsage'
    { cRLSign = Prelude.Nothing,
      dataEncipherment = Prelude.Nothing,
      decipherOnly = Prelude.Nothing,
      digitalSignature = Prelude.Nothing,
      encipherOnly = Prelude.Nothing,
      keyAgreement = Prelude.Nothing,
      keyCertSign = Prelude.Nothing,
      keyEncipherment = Prelude.Nothing,
      nonRepudiation = Prelude.Nothing
    }

-- | Key can be used to sign CRLs.
keyUsage_cRLSign :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_cRLSign = Lens.lens (\KeyUsage' {cRLSign} -> cRLSign) (\s@KeyUsage' {} a -> s {cRLSign = a} :: KeyUsage)

-- | Key can be used to decipher data.
keyUsage_dataEncipherment :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_dataEncipherment = Lens.lens (\KeyUsage' {dataEncipherment} -> dataEncipherment) (\s@KeyUsage' {} a -> s {dataEncipherment = a} :: KeyUsage)

-- | Key can be used only to decipher data.
keyUsage_decipherOnly :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_decipherOnly = Lens.lens (\KeyUsage' {decipherOnly} -> decipherOnly) (\s@KeyUsage' {} a -> s {decipherOnly = a} :: KeyUsage)

-- | Key can be used for digital signing.
keyUsage_digitalSignature :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_digitalSignature = Lens.lens (\KeyUsage' {digitalSignature} -> digitalSignature) (\s@KeyUsage' {} a -> s {digitalSignature = a} :: KeyUsage)

-- | Key can be used only to encipher data.
keyUsage_encipherOnly :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_encipherOnly = Lens.lens (\KeyUsage' {encipherOnly} -> encipherOnly) (\s@KeyUsage' {} a -> s {encipherOnly = a} :: KeyUsage)

-- | Key can be used in a key-agreement protocol.
keyUsage_keyAgreement :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_keyAgreement = Lens.lens (\KeyUsage' {keyAgreement} -> keyAgreement) (\s@KeyUsage' {} a -> s {keyAgreement = a} :: KeyUsage)

-- | Key can be used to sign certificates.
keyUsage_keyCertSign :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_keyCertSign = Lens.lens (\KeyUsage' {keyCertSign} -> keyCertSign) (\s@KeyUsage' {} a -> s {keyCertSign = a} :: KeyUsage)

-- | Key can be used to encipher data.
keyUsage_keyEncipherment :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_keyEncipherment = Lens.lens (\KeyUsage' {keyEncipherment} -> keyEncipherment) (\s@KeyUsage' {} a -> s {keyEncipherment = a} :: KeyUsage)

-- | Key can be used for non-repudiation.
keyUsage_nonRepudiation :: Lens.Lens' KeyUsage (Prelude.Maybe Prelude.Bool)
keyUsage_nonRepudiation = Lens.lens (\KeyUsage' {nonRepudiation} -> nonRepudiation) (\s@KeyUsage' {} a -> s {nonRepudiation = a} :: KeyUsage)

instance Data.FromJSON KeyUsage where
  parseJSON =
    Data.withObject
      "KeyUsage"
      ( \x ->
          KeyUsage'
            Prelude.<$> (x Data..:? "CRLSign")
            Prelude.<*> (x Data..:? "DataEncipherment")
            Prelude.<*> (x Data..:? "DecipherOnly")
            Prelude.<*> (x Data..:? "DigitalSignature")
            Prelude.<*> (x Data..:? "EncipherOnly")
            Prelude.<*> (x Data..:? "KeyAgreement")
            Prelude.<*> (x Data..:? "KeyCertSign")
            Prelude.<*> (x Data..:? "KeyEncipherment")
            Prelude.<*> (x Data..:? "NonRepudiation")
      )

instance Prelude.Hashable KeyUsage where
  hashWithSalt _salt KeyUsage' {..} =
    _salt `Prelude.hashWithSalt` cRLSign
      `Prelude.hashWithSalt` dataEncipherment
      `Prelude.hashWithSalt` decipherOnly
      `Prelude.hashWithSalt` digitalSignature
      `Prelude.hashWithSalt` encipherOnly
      `Prelude.hashWithSalt` keyAgreement
      `Prelude.hashWithSalt` keyCertSign
      `Prelude.hashWithSalt` keyEncipherment
      `Prelude.hashWithSalt` nonRepudiation

instance Prelude.NFData KeyUsage where
  rnf KeyUsage' {..} =
    Prelude.rnf cRLSign
      `Prelude.seq` Prelude.rnf dataEncipherment
      `Prelude.seq` Prelude.rnf decipherOnly
      `Prelude.seq` Prelude.rnf digitalSignature
      `Prelude.seq` Prelude.rnf encipherOnly
      `Prelude.seq` Prelude.rnf keyAgreement
      `Prelude.seq` Prelude.rnf keyCertSign
      `Prelude.seq` Prelude.rnf keyEncipherment
      `Prelude.seq` Prelude.rnf nonRepudiation

instance Data.ToJSON KeyUsage where
  toJSON KeyUsage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CRLSign" Data..=) Prelude.<$> cRLSign,
            ("DataEncipherment" Data..=)
              Prelude.<$> dataEncipherment,
            ("DecipherOnly" Data..=) Prelude.<$> decipherOnly,
            ("DigitalSignature" Data..=)
              Prelude.<$> digitalSignature,
            ("EncipherOnly" Data..=) Prelude.<$> encipherOnly,
            ("KeyAgreement" Data..=) Prelude.<$> keyAgreement,
            ("KeyCertSign" Data..=) Prelude.<$> keyCertSign,
            ("KeyEncipherment" Data..=)
              Prelude.<$> keyEncipherment,
            ("NonRepudiation" Data..=)
              Prelude.<$> nonRepudiation
          ]
      )

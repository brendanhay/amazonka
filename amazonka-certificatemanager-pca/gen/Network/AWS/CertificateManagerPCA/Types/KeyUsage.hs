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
-- Module      : Network.AWS.CertificateManagerPCA.Types.KeyUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.KeyUsage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines one or more purposes for which the key contained in the
-- certificate can be used. Default value for each option is false.
--
-- /See:/ 'newKeyUsage' smart constructor.
data KeyUsage = KeyUsage'
  { -- | Key can be used to decipher data.
    dataEncipherment :: Core.Maybe Core.Bool,
    -- | Key can be used to sign certificates.
    keyCertSign :: Core.Maybe Core.Bool,
    -- | Key can be used to sign CRLs.
    cRLSign :: Core.Maybe Core.Bool,
    -- | Key can be used to encipher data.
    keyEncipherment :: Core.Maybe Core.Bool,
    -- | Key can be used only to encipher data.
    encipherOnly :: Core.Maybe Core.Bool,
    -- | Key can be used in a key-agreement protocol.
    keyAgreement :: Core.Maybe Core.Bool,
    -- | Key can be used for digital signing.
    digitalSignature :: Core.Maybe Core.Bool,
    -- | Key can be used only to decipher data.
    decipherOnly :: Core.Maybe Core.Bool,
    -- | Key can be used for non-repudiation.
    nonRepudiation :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataEncipherment', 'keyUsage_dataEncipherment' - Key can be used to decipher data.
--
-- 'keyCertSign', 'keyUsage_keyCertSign' - Key can be used to sign certificates.
--
-- 'cRLSign', 'keyUsage_cRLSign' - Key can be used to sign CRLs.
--
-- 'keyEncipherment', 'keyUsage_keyEncipherment' - Key can be used to encipher data.
--
-- 'encipherOnly', 'keyUsage_encipherOnly' - Key can be used only to encipher data.
--
-- 'keyAgreement', 'keyUsage_keyAgreement' - Key can be used in a key-agreement protocol.
--
-- 'digitalSignature', 'keyUsage_digitalSignature' - Key can be used for digital signing.
--
-- 'decipherOnly', 'keyUsage_decipherOnly' - Key can be used only to decipher data.
--
-- 'nonRepudiation', 'keyUsage_nonRepudiation' - Key can be used for non-repudiation.
newKeyUsage ::
  KeyUsage
newKeyUsage =
  KeyUsage'
    { dataEncipherment = Core.Nothing,
      keyCertSign = Core.Nothing,
      cRLSign = Core.Nothing,
      keyEncipherment = Core.Nothing,
      encipherOnly = Core.Nothing,
      keyAgreement = Core.Nothing,
      digitalSignature = Core.Nothing,
      decipherOnly = Core.Nothing,
      nonRepudiation = Core.Nothing
    }

-- | Key can be used to decipher data.
keyUsage_dataEncipherment :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_dataEncipherment = Lens.lens (\KeyUsage' {dataEncipherment} -> dataEncipherment) (\s@KeyUsage' {} a -> s {dataEncipherment = a} :: KeyUsage)

-- | Key can be used to sign certificates.
keyUsage_keyCertSign :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_keyCertSign = Lens.lens (\KeyUsage' {keyCertSign} -> keyCertSign) (\s@KeyUsage' {} a -> s {keyCertSign = a} :: KeyUsage)

-- | Key can be used to sign CRLs.
keyUsage_cRLSign :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_cRLSign = Lens.lens (\KeyUsage' {cRLSign} -> cRLSign) (\s@KeyUsage' {} a -> s {cRLSign = a} :: KeyUsage)

-- | Key can be used to encipher data.
keyUsage_keyEncipherment :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_keyEncipherment = Lens.lens (\KeyUsage' {keyEncipherment} -> keyEncipherment) (\s@KeyUsage' {} a -> s {keyEncipherment = a} :: KeyUsage)

-- | Key can be used only to encipher data.
keyUsage_encipherOnly :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_encipherOnly = Lens.lens (\KeyUsage' {encipherOnly} -> encipherOnly) (\s@KeyUsage' {} a -> s {encipherOnly = a} :: KeyUsage)

-- | Key can be used in a key-agreement protocol.
keyUsage_keyAgreement :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_keyAgreement = Lens.lens (\KeyUsage' {keyAgreement} -> keyAgreement) (\s@KeyUsage' {} a -> s {keyAgreement = a} :: KeyUsage)

-- | Key can be used for digital signing.
keyUsage_digitalSignature :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_digitalSignature = Lens.lens (\KeyUsage' {digitalSignature} -> digitalSignature) (\s@KeyUsage' {} a -> s {digitalSignature = a} :: KeyUsage)

-- | Key can be used only to decipher data.
keyUsage_decipherOnly :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_decipherOnly = Lens.lens (\KeyUsage' {decipherOnly} -> decipherOnly) (\s@KeyUsage' {} a -> s {decipherOnly = a} :: KeyUsage)

-- | Key can be used for non-repudiation.
keyUsage_nonRepudiation :: Lens.Lens' KeyUsage (Core.Maybe Core.Bool)
keyUsage_nonRepudiation = Lens.lens (\KeyUsage' {nonRepudiation} -> nonRepudiation) (\s@KeyUsage' {} a -> s {nonRepudiation = a} :: KeyUsage)

instance Core.FromJSON KeyUsage where
  parseJSON =
    Core.withObject
      "KeyUsage"
      ( \x ->
          KeyUsage'
            Core.<$> (x Core..:? "DataEncipherment")
            Core.<*> (x Core..:? "KeyCertSign")
            Core.<*> (x Core..:? "CRLSign")
            Core.<*> (x Core..:? "KeyEncipherment")
            Core.<*> (x Core..:? "EncipherOnly")
            Core.<*> (x Core..:? "KeyAgreement")
            Core.<*> (x Core..:? "DigitalSignature")
            Core.<*> (x Core..:? "DecipherOnly")
            Core.<*> (x Core..:? "NonRepudiation")
      )

instance Core.Hashable KeyUsage

instance Core.NFData KeyUsage

instance Core.ToJSON KeyUsage where
  toJSON KeyUsage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DataEncipherment" Core..=)
              Core.<$> dataEncipherment,
            ("KeyCertSign" Core..=) Core.<$> keyCertSign,
            ("CRLSign" Core..=) Core.<$> cRLSign,
            ("KeyEncipherment" Core..=) Core.<$> keyEncipherment,
            ("EncipherOnly" Core..=) Core.<$> encipherOnly,
            ("KeyAgreement" Core..=) Core.<$> keyAgreement,
            ("DigitalSignature" Core..=)
              Core.<$> digitalSignature,
            ("DecipherOnly" Core..=) Core.<$> decipherOnly,
            ("NonRepudiation" Core..=) Core.<$> nonRepudiation
          ]
      )

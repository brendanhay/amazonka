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
-- Module      : Amazonka.Route53Domains.Types.DnssecSigningAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.DnssecSigningAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a delegation signer (DS) record that was created in
-- the registry by
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AssociateDelegationSignerToDomain.html AssociateDelegationSignerToDomain>.
--
-- /See:/ 'newDnssecSigningAttributes' smart constructor.
data DnssecSigningAttributes = DnssecSigningAttributes'
  { -- | Algorithm which was used to generate the digest from the public key.
    algorithm :: Prelude.Maybe Prelude.Int,
    -- | Defines the type of key. It can be either a KSK (key-signing-key, value
    -- 257) or ZSK (zone-signing-key, value 256). Using KSK is always
    -- encouraged. Only use ZSK if your DNS provider isn\'t Route 53 and you
    -- don’t have KSK available.
    --
    -- If you have KSK and ZSK keys, always use KSK to create a delegations
    -- signer (DS) record. If you have ZSK keys only – use ZSK to create a DS
    -- record.
    flags :: Prelude.Maybe Prelude.Int,
    -- | The base64-encoded public key part of the key pair that is passed to the
    -- registry.
    publicKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnssecSigningAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithm', 'dnssecSigningAttributes_algorithm' - Algorithm which was used to generate the digest from the public key.
--
-- 'flags', 'dnssecSigningAttributes_flags' - Defines the type of key. It can be either a KSK (key-signing-key, value
-- 257) or ZSK (zone-signing-key, value 256). Using KSK is always
-- encouraged. Only use ZSK if your DNS provider isn\'t Route 53 and you
-- don’t have KSK available.
--
-- If you have KSK and ZSK keys, always use KSK to create a delegations
-- signer (DS) record. If you have ZSK keys only – use ZSK to create a DS
-- record.
--
-- 'publicKey', 'dnssecSigningAttributes_publicKey' - The base64-encoded public key part of the key pair that is passed to the
-- registry.
newDnssecSigningAttributes ::
  DnssecSigningAttributes
newDnssecSigningAttributes =
  DnssecSigningAttributes'
    { algorithm =
        Prelude.Nothing,
      flags = Prelude.Nothing,
      publicKey = Prelude.Nothing
    }

-- | Algorithm which was used to generate the digest from the public key.
dnssecSigningAttributes_algorithm :: Lens.Lens' DnssecSigningAttributes (Prelude.Maybe Prelude.Int)
dnssecSigningAttributes_algorithm = Lens.lens (\DnssecSigningAttributes' {algorithm} -> algorithm) (\s@DnssecSigningAttributes' {} a -> s {algorithm = a} :: DnssecSigningAttributes)

-- | Defines the type of key. It can be either a KSK (key-signing-key, value
-- 257) or ZSK (zone-signing-key, value 256). Using KSK is always
-- encouraged. Only use ZSK if your DNS provider isn\'t Route 53 and you
-- don’t have KSK available.
--
-- If you have KSK and ZSK keys, always use KSK to create a delegations
-- signer (DS) record. If you have ZSK keys only – use ZSK to create a DS
-- record.
dnssecSigningAttributes_flags :: Lens.Lens' DnssecSigningAttributes (Prelude.Maybe Prelude.Int)
dnssecSigningAttributes_flags = Lens.lens (\DnssecSigningAttributes' {flags} -> flags) (\s@DnssecSigningAttributes' {} a -> s {flags = a} :: DnssecSigningAttributes)

-- | The base64-encoded public key part of the key pair that is passed to the
-- registry.
dnssecSigningAttributes_publicKey :: Lens.Lens' DnssecSigningAttributes (Prelude.Maybe Prelude.Text)
dnssecSigningAttributes_publicKey = Lens.lens (\DnssecSigningAttributes' {publicKey} -> publicKey) (\s@DnssecSigningAttributes' {} a -> s {publicKey = a} :: DnssecSigningAttributes)

instance Prelude.Hashable DnssecSigningAttributes where
  hashWithSalt _salt DnssecSigningAttributes' {..} =
    _salt `Prelude.hashWithSalt` algorithm
      `Prelude.hashWithSalt` flags
      `Prelude.hashWithSalt` publicKey

instance Prelude.NFData DnssecSigningAttributes where
  rnf DnssecSigningAttributes' {..} =
    Prelude.rnf algorithm
      `Prelude.seq` Prelude.rnf flags
      `Prelude.seq` Prelude.rnf publicKey

instance Data.ToJSON DnssecSigningAttributes where
  toJSON DnssecSigningAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Algorithm" Data..=) Prelude.<$> algorithm,
            ("Flags" Data..=) Prelude.<$> flags,
            ("PublicKey" Data..=) Prelude.<$> publicKey
          ]
      )

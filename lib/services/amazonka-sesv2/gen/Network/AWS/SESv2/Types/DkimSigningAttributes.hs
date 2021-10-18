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
-- Module      : Network.AWS.SESv2.Types.DkimSigningAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.DkimSigningAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains information about the tokens used for setting up
-- Bring Your Own DKIM (BYODKIM).
--
-- /See:/ 'newDkimSigningAttributes' smart constructor.
data DkimSigningAttributes = DkimSigningAttributes'
  { -- | A string that\'s used to identify a public key in the DNS configuration
    -- for a domain.
    domainSigningSelector :: Prelude.Text,
    -- | A private key that\'s used to generate a DKIM signature.
    --
    -- The private key must use 1024-bit RSA encryption, and must be encoded
    -- using base64 encoding.
    domainSigningPrivateKey :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DkimSigningAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainSigningSelector', 'dkimSigningAttributes_domainSigningSelector' - A string that\'s used to identify a public key in the DNS configuration
-- for a domain.
--
-- 'domainSigningPrivateKey', 'dkimSigningAttributes_domainSigningPrivateKey' - A private key that\'s used to generate a DKIM signature.
--
-- The private key must use 1024-bit RSA encryption, and must be encoded
-- using base64 encoding.
newDkimSigningAttributes ::
  -- | 'domainSigningSelector'
  Prelude.Text ->
  -- | 'domainSigningPrivateKey'
  Prelude.Text ->
  DkimSigningAttributes
newDkimSigningAttributes
  pDomainSigningSelector_
  pDomainSigningPrivateKey_ =
    DkimSigningAttributes'
      { domainSigningSelector =
          pDomainSigningSelector_,
        domainSigningPrivateKey =
          Core._Sensitive Lens.# pDomainSigningPrivateKey_
      }

-- | A string that\'s used to identify a public key in the DNS configuration
-- for a domain.
dkimSigningAttributes_domainSigningSelector :: Lens.Lens' DkimSigningAttributes Prelude.Text
dkimSigningAttributes_domainSigningSelector = Lens.lens (\DkimSigningAttributes' {domainSigningSelector} -> domainSigningSelector) (\s@DkimSigningAttributes' {} a -> s {domainSigningSelector = a} :: DkimSigningAttributes)

-- | A private key that\'s used to generate a DKIM signature.
--
-- The private key must use 1024-bit RSA encryption, and must be encoded
-- using base64 encoding.
dkimSigningAttributes_domainSigningPrivateKey :: Lens.Lens' DkimSigningAttributes Prelude.Text
dkimSigningAttributes_domainSigningPrivateKey = Lens.lens (\DkimSigningAttributes' {domainSigningPrivateKey} -> domainSigningPrivateKey) (\s@DkimSigningAttributes' {} a -> s {domainSigningPrivateKey = a} :: DkimSigningAttributes) Prelude.. Core._Sensitive

instance Prelude.Hashable DkimSigningAttributes

instance Prelude.NFData DkimSigningAttributes

instance Core.ToJSON DkimSigningAttributes where
  toJSON DkimSigningAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DomainSigningSelector"
                  Core..= domainSigningSelector
              ),
            Prelude.Just
              ( "DomainSigningPrivateKey"
                  Core..= domainSigningPrivateKey
              )
          ]
      )

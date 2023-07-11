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
-- Module      : Amazonka.IAM.Types.SAMLProviderListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.SAMLProviderListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the list of SAML providers for this account.
--
-- /See:/ 'newSAMLProviderListEntry' smart constructor.
data SAMLProviderListEntry = SAMLProviderListEntry'
  { -- | The Amazon Resource Name (ARN) of the SAML provider.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the SAML provider was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | The expiration date and time for the SAML provider.
    validUntil :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SAMLProviderListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'sAMLProviderListEntry_arn' - The Amazon Resource Name (ARN) of the SAML provider.
--
-- 'createDate', 'sAMLProviderListEntry_createDate' - The date and time when the SAML provider was created.
--
-- 'validUntil', 'sAMLProviderListEntry_validUntil' - The expiration date and time for the SAML provider.
newSAMLProviderListEntry ::
  SAMLProviderListEntry
newSAMLProviderListEntry =
  SAMLProviderListEntry'
    { arn = Prelude.Nothing,
      createDate = Prelude.Nothing,
      validUntil = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the SAML provider.
sAMLProviderListEntry_arn :: Lens.Lens' SAMLProviderListEntry (Prelude.Maybe Prelude.Text)
sAMLProviderListEntry_arn = Lens.lens (\SAMLProviderListEntry' {arn} -> arn) (\s@SAMLProviderListEntry' {} a -> s {arn = a} :: SAMLProviderListEntry)

-- | The date and time when the SAML provider was created.
sAMLProviderListEntry_createDate :: Lens.Lens' SAMLProviderListEntry (Prelude.Maybe Prelude.UTCTime)
sAMLProviderListEntry_createDate = Lens.lens (\SAMLProviderListEntry' {createDate} -> createDate) (\s@SAMLProviderListEntry' {} a -> s {createDate = a} :: SAMLProviderListEntry) Prelude.. Lens.mapping Data._Time

-- | The expiration date and time for the SAML provider.
sAMLProviderListEntry_validUntil :: Lens.Lens' SAMLProviderListEntry (Prelude.Maybe Prelude.UTCTime)
sAMLProviderListEntry_validUntil = Lens.lens (\SAMLProviderListEntry' {validUntil} -> validUntil) (\s@SAMLProviderListEntry' {} a -> s {validUntil = a} :: SAMLProviderListEntry) Prelude.. Lens.mapping Data._Time

instance Data.FromXML SAMLProviderListEntry where
  parseXML x =
    SAMLProviderListEntry'
      Prelude.<$> (x Data..@? "Arn")
      Prelude.<*> (x Data..@? "CreateDate")
      Prelude.<*> (x Data..@? "ValidUntil")

instance Prelude.Hashable SAMLProviderListEntry where
  hashWithSalt _salt SAMLProviderListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` validUntil

instance Prelude.NFData SAMLProviderListEntry where
  rnf SAMLProviderListEntry' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf validUntil

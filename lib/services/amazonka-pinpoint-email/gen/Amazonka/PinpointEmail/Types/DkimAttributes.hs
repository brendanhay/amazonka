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
-- Module      : Amazonka.PinpointEmail.Types.DkimAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.DkimAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types.DkimStatus
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about the DKIM configuration for an
-- email identity.
--
-- /See:/ 'newDkimAttributes' smart constructor.
data DkimAttributes = DkimAttributes'
  { -- | If the value is @true@, then the messages that Amazon Pinpoint sends
    -- from the identity are DKIM-signed. If the value is @false@, then the
    -- messages that Amazon Pinpoint sends from the identity aren\'t
    -- DKIM-signed.
    signingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes whether or not Amazon Pinpoint has successfully located the
    -- DKIM records in the DNS records for the domain. The status can be one of
    -- the following:
    --
    -- -   @PENDING@ – Amazon Pinpoint hasn\'t yet located the DKIM records in
    --     the DNS configuration for the domain, but will continue to attempt
    --     to locate them.
    --
    -- -   @SUCCESS@ – Amazon Pinpoint located the DKIM records in the DNS
    --     configuration for the domain and determined that they\'re correct.
    --     Amazon Pinpoint can now send DKIM-signed email from the identity.
    --
    -- -   @FAILED@ – Amazon Pinpoint was unable to locate the DKIM records in
    --     the DNS settings for the domain, and won\'t continue to search for
    --     them.
    --
    -- -   @TEMPORARY_FAILURE@ – A temporary issue occurred, which prevented
    --     Amazon Pinpoint from determining the DKIM status for the domain.
    --
    -- -   @NOT_STARTED@ – Amazon Pinpoint hasn\'t yet started searching for
    --     the DKIM records in the DKIM records for the domain.
    status :: Prelude.Maybe DkimStatus,
    -- | A set of unique strings that you use to create a set of CNAME records
    -- that you add to the DNS configuration for your domain. When Amazon
    -- Pinpoint detects these records in the DNS configuration for your domain,
    -- the DKIM authentication process is complete. Amazon Pinpoint usually
    -- detects these records within about 72 hours of adding them to the DNS
    -- configuration for your domain.
    tokens :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DkimAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingEnabled', 'dkimAttributes_signingEnabled' - If the value is @true@, then the messages that Amazon Pinpoint sends
-- from the identity are DKIM-signed. If the value is @false@, then the
-- messages that Amazon Pinpoint sends from the identity aren\'t
-- DKIM-signed.
--
-- 'status', 'dkimAttributes_status' - Describes whether or not Amazon Pinpoint has successfully located the
-- DKIM records in the DNS records for the domain. The status can be one of
-- the following:
--
-- -   @PENDING@ – Amazon Pinpoint hasn\'t yet located the DKIM records in
--     the DNS configuration for the domain, but will continue to attempt
--     to locate them.
--
-- -   @SUCCESS@ – Amazon Pinpoint located the DKIM records in the DNS
--     configuration for the domain and determined that they\'re correct.
--     Amazon Pinpoint can now send DKIM-signed email from the identity.
--
-- -   @FAILED@ – Amazon Pinpoint was unable to locate the DKIM records in
--     the DNS settings for the domain, and won\'t continue to search for
--     them.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue occurred, which prevented
--     Amazon Pinpoint from determining the DKIM status for the domain.
--
-- -   @NOT_STARTED@ – Amazon Pinpoint hasn\'t yet started searching for
--     the DKIM records in the DKIM records for the domain.
--
-- 'tokens', 'dkimAttributes_tokens' - A set of unique strings that you use to create a set of CNAME records
-- that you add to the DNS configuration for your domain. When Amazon
-- Pinpoint detects these records in the DNS configuration for your domain,
-- the DKIM authentication process is complete. Amazon Pinpoint usually
-- detects these records within about 72 hours of adding them to the DNS
-- configuration for your domain.
newDkimAttributes ::
  DkimAttributes
newDkimAttributes =
  DkimAttributes'
    { signingEnabled = Prelude.Nothing,
      status = Prelude.Nothing,
      tokens = Prelude.Nothing
    }

-- | If the value is @true@, then the messages that Amazon Pinpoint sends
-- from the identity are DKIM-signed. If the value is @false@, then the
-- messages that Amazon Pinpoint sends from the identity aren\'t
-- DKIM-signed.
dkimAttributes_signingEnabled :: Lens.Lens' DkimAttributes (Prelude.Maybe Prelude.Bool)
dkimAttributes_signingEnabled = Lens.lens (\DkimAttributes' {signingEnabled} -> signingEnabled) (\s@DkimAttributes' {} a -> s {signingEnabled = a} :: DkimAttributes)

-- | Describes whether or not Amazon Pinpoint has successfully located the
-- DKIM records in the DNS records for the domain. The status can be one of
-- the following:
--
-- -   @PENDING@ – Amazon Pinpoint hasn\'t yet located the DKIM records in
--     the DNS configuration for the domain, but will continue to attempt
--     to locate them.
--
-- -   @SUCCESS@ – Amazon Pinpoint located the DKIM records in the DNS
--     configuration for the domain and determined that they\'re correct.
--     Amazon Pinpoint can now send DKIM-signed email from the identity.
--
-- -   @FAILED@ – Amazon Pinpoint was unable to locate the DKIM records in
--     the DNS settings for the domain, and won\'t continue to search for
--     them.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue occurred, which prevented
--     Amazon Pinpoint from determining the DKIM status for the domain.
--
-- -   @NOT_STARTED@ – Amazon Pinpoint hasn\'t yet started searching for
--     the DKIM records in the DKIM records for the domain.
dkimAttributes_status :: Lens.Lens' DkimAttributes (Prelude.Maybe DkimStatus)
dkimAttributes_status = Lens.lens (\DkimAttributes' {status} -> status) (\s@DkimAttributes' {} a -> s {status = a} :: DkimAttributes)

-- | A set of unique strings that you use to create a set of CNAME records
-- that you add to the DNS configuration for your domain. When Amazon
-- Pinpoint detects these records in the DNS configuration for your domain,
-- the DKIM authentication process is complete. Amazon Pinpoint usually
-- detects these records within about 72 hours of adding them to the DNS
-- configuration for your domain.
dkimAttributes_tokens :: Lens.Lens' DkimAttributes (Prelude.Maybe [Prelude.Text])
dkimAttributes_tokens = Lens.lens (\DkimAttributes' {tokens} -> tokens) (\s@DkimAttributes' {} a -> s {tokens = a} :: DkimAttributes) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DkimAttributes where
  parseJSON =
    Data.withObject
      "DkimAttributes"
      ( \x ->
          DkimAttributes'
            Prelude.<$> (x Data..:? "SigningEnabled")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Tokens" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DkimAttributes where
  hashWithSalt _salt DkimAttributes' {..} =
    _salt `Prelude.hashWithSalt` signingEnabled
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tokens

instance Prelude.NFData DkimAttributes where
  rnf DkimAttributes' {..} =
    Prelude.rnf signingEnabled
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tokens

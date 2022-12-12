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
-- Module      : Amazonka.Lightsail.Types.DnsRecordCreationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DnsRecordCreationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.DnsRecordCreationStateCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the creation state of the canonical name (CNAME) records that
-- are automatically added by Amazon Lightsail to the DNS of a domain to
-- validate domain ownership for an SSL\/TLS certificate.
--
-- When you create an SSL\/TLS certificate for a Lightsail resource, you
-- must add a set of CNAME records to the DNS of the domains for the
-- certificate to validate that you own the domains. Lightsail can
-- automatically add the CNAME records to the DNS of the domain if the DNS
-- zone for the domain exists within your Lightsail account. If automatic
-- record addition fails, or if you manage the DNS of your domain using a
-- third-party service, then you must manually add the CNAME records to the
-- DNS of your domain. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/verify-tls-ssl-certificate-using-dns-cname-https Verify an SSL\/TLS certificate in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newDnsRecordCreationState' smart constructor.
data DnsRecordCreationState = DnsRecordCreationState'
  { -- | The status code for the automated DNS record creation.
    --
    -- Following are the possible values:
    --
    -- -   @SUCCEEDED@ - The validation records were successfully added to the
    --     domain.
    --
    -- -   @STARTED@ - The automatic DNS record creation has started.
    --
    -- -   @FAILED@ - The validation records failed to be added to the domain.
    code :: Prelude.Maybe DnsRecordCreationStateCode,
    -- | The message that describes the reason for the status code.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsRecordCreationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'dnsRecordCreationState_code' - The status code for the automated DNS record creation.
--
-- Following are the possible values:
--
-- -   @SUCCEEDED@ - The validation records were successfully added to the
--     domain.
--
-- -   @STARTED@ - The automatic DNS record creation has started.
--
-- -   @FAILED@ - The validation records failed to be added to the domain.
--
-- 'message', 'dnsRecordCreationState_message' - The message that describes the reason for the status code.
newDnsRecordCreationState ::
  DnsRecordCreationState
newDnsRecordCreationState =
  DnsRecordCreationState'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The status code for the automated DNS record creation.
--
-- Following are the possible values:
--
-- -   @SUCCEEDED@ - The validation records were successfully added to the
--     domain.
--
-- -   @STARTED@ - The automatic DNS record creation has started.
--
-- -   @FAILED@ - The validation records failed to be added to the domain.
dnsRecordCreationState_code :: Lens.Lens' DnsRecordCreationState (Prelude.Maybe DnsRecordCreationStateCode)
dnsRecordCreationState_code = Lens.lens (\DnsRecordCreationState' {code} -> code) (\s@DnsRecordCreationState' {} a -> s {code = a} :: DnsRecordCreationState)

-- | The message that describes the reason for the status code.
dnsRecordCreationState_message :: Lens.Lens' DnsRecordCreationState (Prelude.Maybe Prelude.Text)
dnsRecordCreationState_message = Lens.lens (\DnsRecordCreationState' {message} -> message) (\s@DnsRecordCreationState' {} a -> s {message = a} :: DnsRecordCreationState)

instance Data.FromJSON DnsRecordCreationState where
  parseJSON =
    Data.withObject
      "DnsRecordCreationState"
      ( \x ->
          DnsRecordCreationState'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable DnsRecordCreationState where
  hashWithSalt _salt DnsRecordCreationState' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData DnsRecordCreationState where
  rnf DnsRecordCreationState' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message

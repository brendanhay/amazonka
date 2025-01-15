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
-- Module      : Amazonka.Transfer.Types.DescribedSecurityPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedSecurityPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the properties of a security policy that was specified. For
-- more information about security policies, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/security-policies.html Working with security policies>.
--
-- /See:/ 'newDescribedSecurityPolicy' smart constructor.
data DescribedSecurityPolicy = DescribedSecurityPolicy'
  { -- | Specifies whether this policy enables Federal Information Processing
    -- Standards (FIPS).
    fips :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the enabled Secure Shell (SSH) cipher encryption algorithms in
    -- the security policy that is attached to the server.
    sshCiphers :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the enabled SSH key exchange (KEX) encryption algorithms in
    -- the security policy that is attached to the server.
    sshKexs :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the enabled SSH message authentication code (MAC) encryption
    -- algorithms in the security policy that is attached to the server.
    sshMacs :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the enabled Transport Layer Security (TLS) cipher encryption
    -- algorithms in the security policy that is attached to the server.
    tlsCiphers :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the name of the security policy that is attached to the
    -- server.
    securityPolicyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedSecurityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fips', 'describedSecurityPolicy_fips' - Specifies whether this policy enables Federal Information Processing
-- Standards (FIPS).
--
-- 'sshCiphers', 'describedSecurityPolicy_sshCiphers' - Specifies the enabled Secure Shell (SSH) cipher encryption algorithms in
-- the security policy that is attached to the server.
--
-- 'sshKexs', 'describedSecurityPolicy_sshKexs' - Specifies the enabled SSH key exchange (KEX) encryption algorithms in
-- the security policy that is attached to the server.
--
-- 'sshMacs', 'describedSecurityPolicy_sshMacs' - Specifies the enabled SSH message authentication code (MAC) encryption
-- algorithms in the security policy that is attached to the server.
--
-- 'tlsCiphers', 'describedSecurityPolicy_tlsCiphers' - Specifies the enabled Transport Layer Security (TLS) cipher encryption
-- algorithms in the security policy that is attached to the server.
--
-- 'securityPolicyName', 'describedSecurityPolicy_securityPolicyName' - Specifies the name of the security policy that is attached to the
-- server.
newDescribedSecurityPolicy ::
  -- | 'securityPolicyName'
  Prelude.Text ->
  DescribedSecurityPolicy
newDescribedSecurityPolicy pSecurityPolicyName_ =
  DescribedSecurityPolicy'
    { fips = Prelude.Nothing,
      sshCiphers = Prelude.Nothing,
      sshKexs = Prelude.Nothing,
      sshMacs = Prelude.Nothing,
      tlsCiphers = Prelude.Nothing,
      securityPolicyName = pSecurityPolicyName_
    }

-- | Specifies whether this policy enables Federal Information Processing
-- Standards (FIPS).
describedSecurityPolicy_fips :: Lens.Lens' DescribedSecurityPolicy (Prelude.Maybe Prelude.Bool)
describedSecurityPolicy_fips = Lens.lens (\DescribedSecurityPolicy' {fips} -> fips) (\s@DescribedSecurityPolicy' {} a -> s {fips = a} :: DescribedSecurityPolicy)

-- | Specifies the enabled Secure Shell (SSH) cipher encryption algorithms in
-- the security policy that is attached to the server.
describedSecurityPolicy_sshCiphers :: Lens.Lens' DescribedSecurityPolicy (Prelude.Maybe [Prelude.Text])
describedSecurityPolicy_sshCiphers = Lens.lens (\DescribedSecurityPolicy' {sshCiphers} -> sshCiphers) (\s@DescribedSecurityPolicy' {} a -> s {sshCiphers = a} :: DescribedSecurityPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the enabled SSH key exchange (KEX) encryption algorithms in
-- the security policy that is attached to the server.
describedSecurityPolicy_sshKexs :: Lens.Lens' DescribedSecurityPolicy (Prelude.Maybe [Prelude.Text])
describedSecurityPolicy_sshKexs = Lens.lens (\DescribedSecurityPolicy' {sshKexs} -> sshKexs) (\s@DescribedSecurityPolicy' {} a -> s {sshKexs = a} :: DescribedSecurityPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the enabled SSH message authentication code (MAC) encryption
-- algorithms in the security policy that is attached to the server.
describedSecurityPolicy_sshMacs :: Lens.Lens' DescribedSecurityPolicy (Prelude.Maybe [Prelude.Text])
describedSecurityPolicy_sshMacs = Lens.lens (\DescribedSecurityPolicy' {sshMacs} -> sshMacs) (\s@DescribedSecurityPolicy' {} a -> s {sshMacs = a} :: DescribedSecurityPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the enabled Transport Layer Security (TLS) cipher encryption
-- algorithms in the security policy that is attached to the server.
describedSecurityPolicy_tlsCiphers :: Lens.Lens' DescribedSecurityPolicy (Prelude.Maybe [Prelude.Text])
describedSecurityPolicy_tlsCiphers = Lens.lens (\DescribedSecurityPolicy' {tlsCiphers} -> tlsCiphers) (\s@DescribedSecurityPolicy' {} a -> s {tlsCiphers = a} :: DescribedSecurityPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the security policy that is attached to the
-- server.
describedSecurityPolicy_securityPolicyName :: Lens.Lens' DescribedSecurityPolicy Prelude.Text
describedSecurityPolicy_securityPolicyName = Lens.lens (\DescribedSecurityPolicy' {securityPolicyName} -> securityPolicyName) (\s@DescribedSecurityPolicy' {} a -> s {securityPolicyName = a} :: DescribedSecurityPolicy)

instance Data.FromJSON DescribedSecurityPolicy where
  parseJSON =
    Data.withObject
      "DescribedSecurityPolicy"
      ( \x ->
          DescribedSecurityPolicy'
            Prelude.<$> (x Data..:? "Fips")
            Prelude.<*> (x Data..:? "SshCiphers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SshKexs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SshMacs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TlsCiphers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "SecurityPolicyName")
      )

instance Prelude.Hashable DescribedSecurityPolicy where
  hashWithSalt _salt DescribedSecurityPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` fips
      `Prelude.hashWithSalt` sshCiphers
      `Prelude.hashWithSalt` sshKexs
      `Prelude.hashWithSalt` sshMacs
      `Prelude.hashWithSalt` tlsCiphers
      `Prelude.hashWithSalt` securityPolicyName

instance Prelude.NFData DescribedSecurityPolicy where
  rnf DescribedSecurityPolicy' {..} =
    Prelude.rnf fips `Prelude.seq`
      Prelude.rnf sshCiphers `Prelude.seq`
        Prelude.rnf sshKexs `Prelude.seq`
          Prelude.rnf sshMacs `Prelude.seq`
            Prelude.rnf tlsCiphers `Prelude.seq`
              Prelude.rnf securityPolicyName

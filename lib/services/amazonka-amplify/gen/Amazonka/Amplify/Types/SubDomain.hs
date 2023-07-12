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
-- Module      : Amazonka.Amplify.Types.SubDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.SubDomain where

import Amazonka.Amplify.Types.SubDomainSetting
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The subdomain for the domain association.
--
-- /See:/ 'newSubDomain' smart constructor.
data SubDomain = SubDomain'
  { -- | Describes the settings for the subdomain.
    subDomainSetting :: SubDomainSetting,
    -- | The verified status of the subdomain
    verified :: Prelude.Bool,
    -- | The DNS record for the subdomain.
    dnsRecord :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subDomainSetting', 'subDomain_subDomainSetting' - Describes the settings for the subdomain.
--
-- 'verified', 'subDomain_verified' - The verified status of the subdomain
--
-- 'dnsRecord', 'subDomain_dnsRecord' - The DNS record for the subdomain.
newSubDomain ::
  -- | 'subDomainSetting'
  SubDomainSetting ->
  -- | 'verified'
  Prelude.Bool ->
  -- | 'dnsRecord'
  Prelude.Text ->
  SubDomain
newSubDomain
  pSubDomainSetting_
  pVerified_
  pDnsRecord_ =
    SubDomain'
      { subDomainSetting = pSubDomainSetting_,
        verified = pVerified_,
        dnsRecord = pDnsRecord_
      }

-- | Describes the settings for the subdomain.
subDomain_subDomainSetting :: Lens.Lens' SubDomain SubDomainSetting
subDomain_subDomainSetting = Lens.lens (\SubDomain' {subDomainSetting} -> subDomainSetting) (\s@SubDomain' {} a -> s {subDomainSetting = a} :: SubDomain)

-- | The verified status of the subdomain
subDomain_verified :: Lens.Lens' SubDomain Prelude.Bool
subDomain_verified = Lens.lens (\SubDomain' {verified} -> verified) (\s@SubDomain' {} a -> s {verified = a} :: SubDomain)

-- | The DNS record for the subdomain.
subDomain_dnsRecord :: Lens.Lens' SubDomain Prelude.Text
subDomain_dnsRecord = Lens.lens (\SubDomain' {dnsRecord} -> dnsRecord) (\s@SubDomain' {} a -> s {dnsRecord = a} :: SubDomain)

instance Data.FromJSON SubDomain where
  parseJSON =
    Data.withObject
      "SubDomain"
      ( \x ->
          SubDomain'
            Prelude.<$> (x Data..: "subDomainSetting")
            Prelude.<*> (x Data..: "verified")
            Prelude.<*> (x Data..: "dnsRecord")
      )

instance Prelude.Hashable SubDomain where
  hashWithSalt _salt SubDomain' {..} =
    _salt
      `Prelude.hashWithSalt` subDomainSetting
      `Prelude.hashWithSalt` verified
      `Prelude.hashWithSalt` dnsRecord

instance Prelude.NFData SubDomain where
  rnf SubDomain' {..} =
    Prelude.rnf subDomainSetting
      `Prelude.seq` Prelude.rnf verified
      `Prelude.seq` Prelude.rnf dnsRecord

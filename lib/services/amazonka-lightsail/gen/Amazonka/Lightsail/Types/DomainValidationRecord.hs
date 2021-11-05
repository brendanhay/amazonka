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
-- Module      : Amazonka.Lightsail.Types.DomainValidationRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DomainValidationRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.ResourceRecord
import qualified Amazonka.Prelude as Prelude

-- | Describes the domain validation records of an Amazon Lightsail SSL\/TLS
-- certificate.
--
-- /See:/ 'newDomainValidationRecord' smart constructor.
data DomainValidationRecord = DomainValidationRecord'
  { -- | An object that describes the DNS records to add to your domain\'s DNS to
    -- validate it for the certificate.
    resourceRecord :: Prelude.Maybe ResourceRecord,
    -- | The domain name of the certificate validation record. For example,
    -- @example.com@ or @www.example.com@.
    domainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainValidationRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceRecord', 'domainValidationRecord_resourceRecord' - An object that describes the DNS records to add to your domain\'s DNS to
-- validate it for the certificate.
--
-- 'domainName', 'domainValidationRecord_domainName' - The domain name of the certificate validation record. For example,
-- @example.com@ or @www.example.com@.
newDomainValidationRecord ::
  DomainValidationRecord
newDomainValidationRecord =
  DomainValidationRecord'
    { resourceRecord =
        Prelude.Nothing,
      domainName = Prelude.Nothing
    }

-- | An object that describes the DNS records to add to your domain\'s DNS to
-- validate it for the certificate.
domainValidationRecord_resourceRecord :: Lens.Lens' DomainValidationRecord (Prelude.Maybe ResourceRecord)
domainValidationRecord_resourceRecord = Lens.lens (\DomainValidationRecord' {resourceRecord} -> resourceRecord) (\s@DomainValidationRecord' {} a -> s {resourceRecord = a} :: DomainValidationRecord)

-- | The domain name of the certificate validation record. For example,
-- @example.com@ or @www.example.com@.
domainValidationRecord_domainName :: Lens.Lens' DomainValidationRecord (Prelude.Maybe Prelude.Text)
domainValidationRecord_domainName = Lens.lens (\DomainValidationRecord' {domainName} -> domainName) (\s@DomainValidationRecord' {} a -> s {domainName = a} :: DomainValidationRecord)

instance Core.FromJSON DomainValidationRecord where
  parseJSON =
    Core.withObject
      "DomainValidationRecord"
      ( \x ->
          DomainValidationRecord'
            Prelude.<$> (x Core..:? "resourceRecord")
            Prelude.<*> (x Core..:? "domainName")
      )

instance Prelude.Hashable DomainValidationRecord

instance Prelude.NFData DomainValidationRecord

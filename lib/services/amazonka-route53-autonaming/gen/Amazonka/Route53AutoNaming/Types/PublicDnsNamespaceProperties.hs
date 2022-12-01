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
-- Module      : Amazonka.Route53AutoNaming.Types.PublicDnsNamespaceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PublicDnsNamespaceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.PublicDnsPropertiesMutable

-- | DNS properties for the public DNS namespace.
--
-- /See:/ 'newPublicDnsNamespaceProperties' smart constructor.
data PublicDnsNamespaceProperties = PublicDnsNamespaceProperties'
  { -- | DNS properties for the public DNS namespace.
    dnsProperties :: PublicDnsPropertiesMutable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicDnsNamespaceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsProperties', 'publicDnsNamespaceProperties_dnsProperties' - DNS properties for the public DNS namespace.
newPublicDnsNamespaceProperties ::
  -- | 'dnsProperties'
  PublicDnsPropertiesMutable ->
  PublicDnsNamespaceProperties
newPublicDnsNamespaceProperties pDnsProperties_ =
  PublicDnsNamespaceProperties'
    { dnsProperties =
        pDnsProperties_
    }

-- | DNS properties for the public DNS namespace.
publicDnsNamespaceProperties_dnsProperties :: Lens.Lens' PublicDnsNamespaceProperties PublicDnsPropertiesMutable
publicDnsNamespaceProperties_dnsProperties = Lens.lens (\PublicDnsNamespaceProperties' {dnsProperties} -> dnsProperties) (\s@PublicDnsNamespaceProperties' {} a -> s {dnsProperties = a} :: PublicDnsNamespaceProperties)

instance
  Prelude.Hashable
    PublicDnsNamespaceProperties
  where
  hashWithSalt _salt PublicDnsNamespaceProperties' {..} =
    _salt `Prelude.hashWithSalt` dnsProperties

instance Prelude.NFData PublicDnsNamespaceProperties where
  rnf PublicDnsNamespaceProperties' {..} =
    Prelude.rnf dnsProperties

instance Core.ToJSON PublicDnsNamespaceProperties where
  toJSON PublicDnsNamespaceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DnsProperties" Core..= dnsProperties)
          ]
      )

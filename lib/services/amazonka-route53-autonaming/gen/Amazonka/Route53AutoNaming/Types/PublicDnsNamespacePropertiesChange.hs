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
-- Module      : Amazonka.Route53AutoNaming.Types.PublicDnsNamespacePropertiesChange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PublicDnsNamespacePropertiesChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.PublicDnsPropertiesMutableChange

-- | Updated properties for the public DNS namespace.
--
-- /See:/ 'newPublicDnsNamespacePropertiesChange' smart constructor.
data PublicDnsNamespacePropertiesChange = PublicDnsNamespacePropertiesChange'
  { -- | Updated DNS properties for the hosted zone for the public DNS namespace.
    dnsProperties :: PublicDnsPropertiesMutableChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicDnsNamespacePropertiesChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsProperties', 'publicDnsNamespacePropertiesChange_dnsProperties' - Updated DNS properties for the hosted zone for the public DNS namespace.
newPublicDnsNamespacePropertiesChange ::
  -- | 'dnsProperties'
  PublicDnsPropertiesMutableChange ->
  PublicDnsNamespacePropertiesChange
newPublicDnsNamespacePropertiesChange pDnsProperties_ =
  PublicDnsNamespacePropertiesChange'
    { dnsProperties =
        pDnsProperties_
    }

-- | Updated DNS properties for the hosted zone for the public DNS namespace.
publicDnsNamespacePropertiesChange_dnsProperties :: Lens.Lens' PublicDnsNamespacePropertiesChange PublicDnsPropertiesMutableChange
publicDnsNamespacePropertiesChange_dnsProperties = Lens.lens (\PublicDnsNamespacePropertiesChange' {dnsProperties} -> dnsProperties) (\s@PublicDnsNamespacePropertiesChange' {} a -> s {dnsProperties = a} :: PublicDnsNamespacePropertiesChange)

instance
  Prelude.Hashable
    PublicDnsNamespacePropertiesChange
  where
  hashWithSalt
    _salt
    PublicDnsNamespacePropertiesChange' {..} =
      _salt `Prelude.hashWithSalt` dnsProperties

instance
  Prelude.NFData
    PublicDnsNamespacePropertiesChange
  where
  rnf PublicDnsNamespacePropertiesChange' {..} =
    Prelude.rnf dnsProperties

instance
  Data.ToJSON
    PublicDnsNamespacePropertiesChange
  where
  toJSON PublicDnsNamespacePropertiesChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DnsProperties" Data..= dnsProperties)
          ]
      )

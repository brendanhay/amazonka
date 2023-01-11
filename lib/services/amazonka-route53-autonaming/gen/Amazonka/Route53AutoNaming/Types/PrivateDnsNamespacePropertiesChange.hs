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
-- Module      : Amazonka.Route53AutoNaming.Types.PrivateDnsNamespacePropertiesChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PrivateDnsNamespacePropertiesChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.PrivateDnsPropertiesMutableChange

-- | Updated properties for the private DNS namespace.
--
-- /See:/ 'newPrivateDnsNamespacePropertiesChange' smart constructor.
data PrivateDnsNamespacePropertiesChange = PrivateDnsNamespacePropertiesChange'
  { -- | Updated DNS properties for the private DNS namespace.
    dnsProperties :: PrivateDnsPropertiesMutableChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsNamespacePropertiesChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsProperties', 'privateDnsNamespacePropertiesChange_dnsProperties' - Updated DNS properties for the private DNS namespace.
newPrivateDnsNamespacePropertiesChange ::
  -- | 'dnsProperties'
  PrivateDnsPropertiesMutableChange ->
  PrivateDnsNamespacePropertiesChange
newPrivateDnsNamespacePropertiesChange
  pDnsProperties_ =
    PrivateDnsNamespacePropertiesChange'
      { dnsProperties =
          pDnsProperties_
      }

-- | Updated DNS properties for the private DNS namespace.
privateDnsNamespacePropertiesChange_dnsProperties :: Lens.Lens' PrivateDnsNamespacePropertiesChange PrivateDnsPropertiesMutableChange
privateDnsNamespacePropertiesChange_dnsProperties = Lens.lens (\PrivateDnsNamespacePropertiesChange' {dnsProperties} -> dnsProperties) (\s@PrivateDnsNamespacePropertiesChange' {} a -> s {dnsProperties = a} :: PrivateDnsNamespacePropertiesChange)

instance
  Prelude.Hashable
    PrivateDnsNamespacePropertiesChange
  where
  hashWithSalt
    _salt
    PrivateDnsNamespacePropertiesChange' {..} =
      _salt `Prelude.hashWithSalt` dnsProperties

instance
  Prelude.NFData
    PrivateDnsNamespacePropertiesChange
  where
  rnf PrivateDnsNamespacePropertiesChange' {..} =
    Prelude.rnf dnsProperties

instance
  Data.ToJSON
    PrivateDnsNamespacePropertiesChange
  where
  toJSON PrivateDnsNamespacePropertiesChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DnsProperties" Data..= dnsProperties)
          ]
      )

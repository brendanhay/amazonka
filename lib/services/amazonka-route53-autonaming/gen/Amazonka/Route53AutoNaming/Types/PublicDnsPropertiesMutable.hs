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
-- Module      : Amazonka.Route53AutoNaming.Types.PublicDnsPropertiesMutable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PublicDnsPropertiesMutable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.SOA

-- | DNS properties for the public DNS namespace.
--
-- /See:/ 'newPublicDnsPropertiesMutable' smart constructor.
data PublicDnsPropertiesMutable = PublicDnsPropertiesMutable'
  { -- | Start of Authority (SOA) record for the hosted zone for the public DNS
    -- namespace.
    soa :: SOA
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicDnsPropertiesMutable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'soa', 'publicDnsPropertiesMutable_soa' - Start of Authority (SOA) record for the hosted zone for the public DNS
-- namespace.
newPublicDnsPropertiesMutable ::
  -- | 'soa'
  SOA ->
  PublicDnsPropertiesMutable
newPublicDnsPropertiesMutable pSOA_ =
  PublicDnsPropertiesMutable' {soa = pSOA_}

-- | Start of Authority (SOA) record for the hosted zone for the public DNS
-- namespace.
publicDnsPropertiesMutable_soa :: Lens.Lens' PublicDnsPropertiesMutable SOA
publicDnsPropertiesMutable_soa = Lens.lens (\PublicDnsPropertiesMutable' {soa} -> soa) (\s@PublicDnsPropertiesMutable' {} a -> s {soa = a} :: PublicDnsPropertiesMutable)

instance Prelude.Hashable PublicDnsPropertiesMutable where
  hashWithSalt _salt PublicDnsPropertiesMutable' {..} =
    _salt `Prelude.hashWithSalt` soa

instance Prelude.NFData PublicDnsPropertiesMutable where
  rnf PublicDnsPropertiesMutable' {..} = Prelude.rnf soa

instance Data.ToJSON PublicDnsPropertiesMutable where
  toJSON PublicDnsPropertiesMutable' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SOA" Data..= soa)]
      )

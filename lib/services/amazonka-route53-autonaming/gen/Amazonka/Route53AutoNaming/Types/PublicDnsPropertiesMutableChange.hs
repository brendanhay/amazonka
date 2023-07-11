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
-- Module      : Amazonka.Route53AutoNaming.Types.PublicDnsPropertiesMutableChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PublicDnsPropertiesMutableChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.SOAChange

-- | Updated DNS properties for the public DNS namespace.
--
-- /See:/ 'newPublicDnsPropertiesMutableChange' smart constructor.
data PublicDnsPropertiesMutableChange = PublicDnsPropertiesMutableChange'
  { -- | Updated fields for the Start of Authority (SOA) record for the hosted
    -- zone for the public DNS namespace.
    soa :: SOAChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicDnsPropertiesMutableChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'soa', 'publicDnsPropertiesMutableChange_soa' - Updated fields for the Start of Authority (SOA) record for the hosted
-- zone for the public DNS namespace.
newPublicDnsPropertiesMutableChange ::
  -- | 'soa'
  SOAChange ->
  PublicDnsPropertiesMutableChange
newPublicDnsPropertiesMutableChange pSOA_ =
  PublicDnsPropertiesMutableChange' {soa = pSOA_}

-- | Updated fields for the Start of Authority (SOA) record for the hosted
-- zone for the public DNS namespace.
publicDnsPropertiesMutableChange_soa :: Lens.Lens' PublicDnsPropertiesMutableChange SOAChange
publicDnsPropertiesMutableChange_soa = Lens.lens (\PublicDnsPropertiesMutableChange' {soa} -> soa) (\s@PublicDnsPropertiesMutableChange' {} a -> s {soa = a} :: PublicDnsPropertiesMutableChange)

instance
  Prelude.Hashable
    PublicDnsPropertiesMutableChange
  where
  hashWithSalt
    _salt
    PublicDnsPropertiesMutableChange' {..} =
      _salt `Prelude.hashWithSalt` soa

instance
  Prelude.NFData
    PublicDnsPropertiesMutableChange
  where
  rnf PublicDnsPropertiesMutableChange' {..} =
    Prelude.rnf soa

instance Data.ToJSON PublicDnsPropertiesMutableChange where
  toJSON PublicDnsPropertiesMutableChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SOA" Data..= soa)]
      )

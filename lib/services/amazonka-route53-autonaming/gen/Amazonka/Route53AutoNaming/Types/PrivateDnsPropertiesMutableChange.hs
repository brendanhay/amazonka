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
-- Module      : Amazonka.Route53AutoNaming.Types.PrivateDnsPropertiesMutableChange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PrivateDnsPropertiesMutableChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.SOAChange

-- | Updated DNS properties for the private DNS namespace.
--
-- /See:/ 'newPrivateDnsPropertiesMutableChange' smart constructor.
data PrivateDnsPropertiesMutableChange = PrivateDnsPropertiesMutableChange'
  { -- | Updated fields for the Start of Authority (SOA) record for the hosted
    -- zone for the private DNS namespace.
    soa :: SOAChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsPropertiesMutableChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'soa', 'privateDnsPropertiesMutableChange_soa' - Updated fields for the Start of Authority (SOA) record for the hosted
-- zone for the private DNS namespace.
newPrivateDnsPropertiesMutableChange ::
  -- | 'soa'
  SOAChange ->
  PrivateDnsPropertiesMutableChange
newPrivateDnsPropertiesMutableChange pSOA_ =
  PrivateDnsPropertiesMutableChange' {soa = pSOA_}

-- | Updated fields for the Start of Authority (SOA) record for the hosted
-- zone for the private DNS namespace.
privateDnsPropertiesMutableChange_soa :: Lens.Lens' PrivateDnsPropertiesMutableChange SOAChange
privateDnsPropertiesMutableChange_soa = Lens.lens (\PrivateDnsPropertiesMutableChange' {soa} -> soa) (\s@PrivateDnsPropertiesMutableChange' {} a -> s {soa = a} :: PrivateDnsPropertiesMutableChange)

instance
  Prelude.Hashable
    PrivateDnsPropertiesMutableChange
  where
  hashWithSalt
    _salt
    PrivateDnsPropertiesMutableChange' {..} =
      _salt `Prelude.hashWithSalt` soa

instance
  Prelude.NFData
    PrivateDnsPropertiesMutableChange
  where
  rnf PrivateDnsPropertiesMutableChange' {..} =
    Prelude.rnf soa

instance
  Core.ToJSON
    PrivateDnsPropertiesMutableChange
  where
  toJSON PrivateDnsPropertiesMutableChange' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("SOA" Core..= soa)]
      )

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
-- Module      : Amazonka.Route53AutoNaming.Types.PrivateDnsPropertiesMutable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.PrivateDnsPropertiesMutable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.SOA

-- | DNS properties for the private DNS namespace.
--
-- /See:/ 'newPrivateDnsPropertiesMutable' smart constructor.
data PrivateDnsPropertiesMutable = PrivateDnsPropertiesMutable'
  { -- | Fields for the Start of Authority (SOA) record for the hosted zone for
    -- the private DNS namespace.
    soa :: SOA
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsPropertiesMutable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'soa', 'privateDnsPropertiesMutable_soa' - Fields for the Start of Authority (SOA) record for the hosted zone for
-- the private DNS namespace.
newPrivateDnsPropertiesMutable ::
  -- | 'soa'
  SOA ->
  PrivateDnsPropertiesMutable
newPrivateDnsPropertiesMutable pSOA_ =
  PrivateDnsPropertiesMutable' {soa = pSOA_}

-- | Fields for the Start of Authority (SOA) record for the hosted zone for
-- the private DNS namespace.
privateDnsPropertiesMutable_soa :: Lens.Lens' PrivateDnsPropertiesMutable SOA
privateDnsPropertiesMutable_soa = Lens.lens (\PrivateDnsPropertiesMutable' {soa} -> soa) (\s@PrivateDnsPropertiesMutable' {} a -> s {soa = a} :: PrivateDnsPropertiesMutable)

instance Prelude.Hashable PrivateDnsPropertiesMutable where
  hashWithSalt _salt PrivateDnsPropertiesMutable' {..} =
    _salt `Prelude.hashWithSalt` soa

instance Prelude.NFData PrivateDnsPropertiesMutable where
  rnf PrivateDnsPropertiesMutable' {..} =
    Prelude.rnf soa

instance Data.ToJSON PrivateDnsPropertiesMutable where
  toJSON PrivateDnsPropertiesMutable' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SOA" Data..= soa)]
      )

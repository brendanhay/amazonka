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
-- Module      : Amazonka.Route53AutoNaming.Types.SOA
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.SOA where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Start of Authority (SOA) properties for a public or private DNS
-- namespace.
--
-- /See:/ 'newSOA' smart constructor.
data SOA = SOA'
  { -- | The time to live (TTL) for purposes of negative caching.
    ttl :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SOA' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ttl', 'soa_ttl' - The time to live (TTL) for purposes of negative caching.
newSOA ::
  -- | 'ttl'
  Prelude.Natural ->
  SOA
newSOA pTTL_ = SOA' {ttl = pTTL_}

-- | The time to live (TTL) for purposes of negative caching.
soa_ttl :: Lens.Lens' SOA Prelude.Natural
soa_ttl = Lens.lens (\SOA' {ttl} -> ttl) (\s@SOA' {} a -> s {ttl = a} :: SOA)

instance Data.FromJSON SOA where
  parseJSON =
    Data.withObject
      "SOA"
      (\x -> SOA' Prelude.<$> (x Data..: "TTL"))

instance Prelude.Hashable SOA where
  hashWithSalt _salt SOA' {..} =
    _salt `Prelude.hashWithSalt` ttl

instance Prelude.NFData SOA where
  rnf SOA' {..} = Prelude.rnf ttl

instance Data.ToJSON SOA where
  toJSON SOA' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TTL" Data..= ttl)]
      )

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
-- Module      : Amazonka.Route53AutoNaming.Types.SOAChange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.SOAChange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Updated Start of Authority (SOA) properties for a public or private DNS
-- namespace.
--
-- /See:/ 'newSOAChange' smart constructor.
data SOAChange = SOAChange'
  { -- | The updated time to live (TTL) for purposes of negative caching.
    ttl :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SOAChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ttl', 'sOAChange_ttl' - The updated time to live (TTL) for purposes of negative caching.
newSOAChange ::
  -- | 'ttl'
  Prelude.Natural ->
  SOAChange
newSOAChange pTTL_ = SOAChange' {ttl = pTTL_}

-- | The updated time to live (TTL) for purposes of negative caching.
sOAChange_ttl :: Lens.Lens' SOAChange Prelude.Natural
sOAChange_ttl = Lens.lens (\SOAChange' {ttl} -> ttl) (\s@SOAChange' {} a -> s {ttl = a} :: SOAChange)

instance Prelude.Hashable SOAChange where
  hashWithSalt _salt SOAChange' {..} =
    _salt `Prelude.hashWithSalt` ttl

instance Prelude.NFData SOAChange where
  rnf SOAChange' {..} = Prelude.rnf ttl

instance Core.ToJSON SOAChange where
  toJSON SOAChange' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TTL" Core..= ttl)]
      )

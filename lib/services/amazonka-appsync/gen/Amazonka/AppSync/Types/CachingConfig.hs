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
-- Module      : Amazonka.AppSync.Types.CachingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.CachingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The caching configuration for a resolver that has caching activated.
--
-- /See:/ 'newCachingConfig' smart constructor.
data CachingConfig = CachingConfig'
  { -- | The caching keys for a resolver that has caching activated.
    --
    -- Valid values are entries from the @$context.arguments@,
    -- @$context.source@, and @$context.identity@ maps.
    cachingKeys :: Prelude.Maybe [Prelude.Text],
    -- | The TTL in seconds for a resolver that has caching activated.
    --
    -- Valid values are 1–3,600 seconds.
    ttl :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CachingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachingKeys', 'cachingConfig_cachingKeys' - The caching keys for a resolver that has caching activated.
--
-- Valid values are entries from the @$context.arguments@,
-- @$context.source@, and @$context.identity@ maps.
--
-- 'ttl', 'cachingConfig_ttl' - The TTL in seconds for a resolver that has caching activated.
--
-- Valid values are 1–3,600 seconds.
newCachingConfig ::
  -- | 'ttl'
  Prelude.Integer ->
  CachingConfig
newCachingConfig pTtl_ =
  CachingConfig'
    { cachingKeys = Prelude.Nothing,
      ttl = pTtl_
    }

-- | The caching keys for a resolver that has caching activated.
--
-- Valid values are entries from the @$context.arguments@,
-- @$context.source@, and @$context.identity@ maps.
cachingConfig_cachingKeys :: Lens.Lens' CachingConfig (Prelude.Maybe [Prelude.Text])
cachingConfig_cachingKeys = Lens.lens (\CachingConfig' {cachingKeys} -> cachingKeys) (\s@CachingConfig' {} a -> s {cachingKeys = a} :: CachingConfig) Prelude.. Lens.mapping Lens.coerced

-- | The TTL in seconds for a resolver that has caching activated.
--
-- Valid values are 1–3,600 seconds.
cachingConfig_ttl :: Lens.Lens' CachingConfig Prelude.Integer
cachingConfig_ttl = Lens.lens (\CachingConfig' {ttl} -> ttl) (\s@CachingConfig' {} a -> s {ttl = a} :: CachingConfig)

instance Data.FromJSON CachingConfig where
  parseJSON =
    Data.withObject
      "CachingConfig"
      ( \x ->
          CachingConfig'
            Prelude.<$> (x Data..:? "cachingKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ttl")
      )

instance Prelude.Hashable CachingConfig where
  hashWithSalt _salt CachingConfig' {..} =
    _salt `Prelude.hashWithSalt` cachingKeys
      `Prelude.hashWithSalt` ttl

instance Prelude.NFData CachingConfig where
  rnf CachingConfig' {..} =
    Prelude.rnf cachingKeys
      `Prelude.seq` Prelude.rnf ttl

instance Data.ToJSON CachingConfig where
  toJSON CachingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cachingKeys" Data..=) Prelude.<$> cachingKeys,
            Prelude.Just ("ttl" Data..= ttl)
          ]
      )

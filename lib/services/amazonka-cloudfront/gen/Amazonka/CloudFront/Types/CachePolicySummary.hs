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
-- Module      : Amazonka.CloudFront.Types.CachePolicySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CachePolicySummary where

import Amazonka.CloudFront.Types.CachePolicy
import Amazonka.CloudFront.Types.CachePolicyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a cache policy.
--
-- /See:/ 'newCachePolicySummary' smart constructor.
data CachePolicySummary = CachePolicySummary'
  { -- | The type of cache policy, either @managed@ (created by Amazon Web
    -- Services) or @custom@ (created in this Amazon Web Services account).
    type' :: CachePolicyType,
    -- | The cache policy.
    cachePolicy :: CachePolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CachePolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'cachePolicySummary_type' - The type of cache policy, either @managed@ (created by Amazon Web
-- Services) or @custom@ (created in this Amazon Web Services account).
--
-- 'cachePolicy', 'cachePolicySummary_cachePolicy' - The cache policy.
newCachePolicySummary ::
  -- | 'type''
  CachePolicyType ->
  -- | 'cachePolicy'
  CachePolicy ->
  CachePolicySummary
newCachePolicySummary pType_ pCachePolicy_ =
  CachePolicySummary'
    { type' = pType_,
      cachePolicy = pCachePolicy_
    }

-- | The type of cache policy, either @managed@ (created by Amazon Web
-- Services) or @custom@ (created in this Amazon Web Services account).
cachePolicySummary_type :: Lens.Lens' CachePolicySummary CachePolicyType
cachePolicySummary_type = Lens.lens (\CachePolicySummary' {type'} -> type') (\s@CachePolicySummary' {} a -> s {type' = a} :: CachePolicySummary)

-- | The cache policy.
cachePolicySummary_cachePolicy :: Lens.Lens' CachePolicySummary CachePolicy
cachePolicySummary_cachePolicy = Lens.lens (\CachePolicySummary' {cachePolicy} -> cachePolicy) (\s@CachePolicySummary' {} a -> s {cachePolicy = a} :: CachePolicySummary)

instance Data.FromXML CachePolicySummary where
  parseXML x =
    CachePolicySummary'
      Prelude.<$> (x Data..@ "Type")
      Prelude.<*> (x Data..@ "CachePolicy")

instance Prelude.Hashable CachePolicySummary where
  hashWithSalt _salt CachePolicySummary' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` cachePolicy

instance Prelude.NFData CachePolicySummary where
  rnf CachePolicySummary' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf cachePolicy

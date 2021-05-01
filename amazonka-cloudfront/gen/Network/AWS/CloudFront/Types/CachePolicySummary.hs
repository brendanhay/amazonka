{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.CachePolicySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicySummary where

import Network.AWS.CloudFront.Types.CachePolicy
import Network.AWS.CloudFront.Types.CachePolicyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains a cache policy.
--
-- /See:/ 'newCachePolicySummary' smart constructor.
data CachePolicySummary = CachePolicySummary'
  { -- | The type of cache policy, either @managed@ (created by AWS) or @custom@
    -- (created in this AWS account).
    type' :: CachePolicyType,
    -- | The cache policy.
    cachePolicy :: CachePolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CachePolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'cachePolicySummary_type' - The type of cache policy, either @managed@ (created by AWS) or @custom@
-- (created in this AWS account).
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

-- | The type of cache policy, either @managed@ (created by AWS) or @custom@
-- (created in this AWS account).
cachePolicySummary_type :: Lens.Lens' CachePolicySummary CachePolicyType
cachePolicySummary_type = Lens.lens (\CachePolicySummary' {type'} -> type') (\s@CachePolicySummary' {} a -> s {type' = a} :: CachePolicySummary)

-- | The cache policy.
cachePolicySummary_cachePolicy :: Lens.Lens' CachePolicySummary CachePolicy
cachePolicySummary_cachePolicy = Lens.lens (\CachePolicySummary' {cachePolicy} -> cachePolicy) (\s@CachePolicySummary' {} a -> s {cachePolicy = a} :: CachePolicySummary)

instance Prelude.FromXML CachePolicySummary where
  parseXML x =
    CachePolicySummary'
      Prelude.<$> (x Prelude..@ "Type")
      Prelude.<*> (x Prelude..@ "CachePolicy")

instance Prelude.Hashable CachePolicySummary

instance Prelude.NFData CachePolicySummary

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
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A value that applies only to a certain cache node type.
--
-- /See:/ 'newCacheNodeTypeSpecificValue' smart constructor.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue'
  { -- | The cache node type for which this value applies.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The value for the cache node type.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CacheNodeTypeSpecificValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheNodeType', 'cacheNodeTypeSpecificValue_cacheNodeType' - The cache node type for which this value applies.
--
-- 'value', 'cacheNodeTypeSpecificValue_value' - The value for the cache node type.
newCacheNodeTypeSpecificValue ::
  CacheNodeTypeSpecificValue
newCacheNodeTypeSpecificValue =
  CacheNodeTypeSpecificValue'
    { cacheNodeType =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The cache node type for which this value applies.
cacheNodeTypeSpecificValue_cacheNodeType :: Lens.Lens' CacheNodeTypeSpecificValue (Prelude.Maybe Prelude.Text)
cacheNodeTypeSpecificValue_cacheNodeType = Lens.lens (\CacheNodeTypeSpecificValue' {cacheNodeType} -> cacheNodeType) (\s@CacheNodeTypeSpecificValue' {} a -> s {cacheNodeType = a} :: CacheNodeTypeSpecificValue)

-- | The value for the cache node type.
cacheNodeTypeSpecificValue_value :: Lens.Lens' CacheNodeTypeSpecificValue (Prelude.Maybe Prelude.Text)
cacheNodeTypeSpecificValue_value = Lens.lens (\CacheNodeTypeSpecificValue' {value} -> value) (\s@CacheNodeTypeSpecificValue' {} a -> s {value = a} :: CacheNodeTypeSpecificValue)

instance Prelude.FromXML CacheNodeTypeSpecificValue where
  parseXML x =
    CacheNodeTypeSpecificValue'
      Prelude.<$> (x Prelude..@? "CacheNodeType")
      Prelude.<*> (x Prelude..@? "Value")

instance Prelude.Hashable CacheNodeTypeSpecificValue

instance Prelude.NFData CacheNodeTypeSpecificValue

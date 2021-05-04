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
-- Module      : Network.AWS.StorageGateway.Types.CacheAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.CacheAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Lists refresh cache information.
--
-- /See:/ 'newCacheAttributes' smart constructor.
data CacheAttributes = CacheAttributes'
  { -- | Refreshes a file share\'s cache by using Time To Live (TTL). TTL is the
    -- length of time since the last refresh after which access to the
    -- directory would cause the file gateway to first refresh that
    -- directory\'s contents from the Amazon S3 bucket. The TTL duration is in
    -- seconds.
    --
    -- Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
    cacheStaleTimeoutInSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CacheAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheStaleTimeoutInSeconds', 'cacheAttributes_cacheStaleTimeoutInSeconds' - Refreshes a file share\'s cache by using Time To Live (TTL). TTL is the
-- length of time since the last refresh after which access to the
-- directory would cause the file gateway to first refresh that
-- directory\'s contents from the Amazon S3 bucket. The TTL duration is in
-- seconds.
--
-- Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
newCacheAttributes ::
  CacheAttributes
newCacheAttributes =
  CacheAttributes'
    { cacheStaleTimeoutInSeconds =
        Prelude.Nothing
    }

-- | Refreshes a file share\'s cache by using Time To Live (TTL). TTL is the
-- length of time since the last refresh after which access to the
-- directory would cause the file gateway to first refresh that
-- directory\'s contents from the Amazon S3 bucket. The TTL duration is in
-- seconds.
--
-- Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
cacheAttributes_cacheStaleTimeoutInSeconds :: Lens.Lens' CacheAttributes (Prelude.Maybe Prelude.Int)
cacheAttributes_cacheStaleTimeoutInSeconds = Lens.lens (\CacheAttributes' {cacheStaleTimeoutInSeconds} -> cacheStaleTimeoutInSeconds) (\s@CacheAttributes' {} a -> s {cacheStaleTimeoutInSeconds = a} :: CacheAttributes)

instance Prelude.FromJSON CacheAttributes where
  parseJSON =
    Prelude.withObject
      "CacheAttributes"
      ( \x ->
          CacheAttributes'
            Prelude.<$> (x Prelude..:? "CacheStaleTimeoutInSeconds")
      )

instance Prelude.Hashable CacheAttributes

instance Prelude.NFData CacheAttributes

instance Prelude.ToJSON CacheAttributes where
  toJSON CacheAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CacheStaleTimeoutInSeconds" Prelude..=)
              Prelude.<$> cacheStaleTimeoutInSeconds
          ]
      )

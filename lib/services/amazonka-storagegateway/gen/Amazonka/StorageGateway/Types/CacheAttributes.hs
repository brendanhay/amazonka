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
-- Module      : Amazonka.StorageGateway.Types.CacheAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.CacheAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The refresh cache information for the file share or FSx file systems.
--
-- /See:/ 'newCacheAttributes' smart constructor.
data CacheAttributes = CacheAttributes'
  { -- | Refreshes a file share\'s cache by using Time To Live (TTL). TTL is the
    -- length of time since the last refresh after which access to the
    -- directory would cause the file gateway to first refresh that
    -- directory\'s contents from the Amazon S3 bucket or Amazon FSx file
    -- system. The TTL duration is in seconds.
    --
    -- Valid Values:0, 300 to 2,592,000 seconds (5 minutes to 30 days)
    cacheStaleTimeoutInSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- directory\'s contents from the Amazon S3 bucket or Amazon FSx file
-- system. The TTL duration is in seconds.
--
-- Valid Values:0, 300 to 2,592,000 seconds (5 minutes to 30 days)
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
-- directory\'s contents from the Amazon S3 bucket or Amazon FSx file
-- system. The TTL duration is in seconds.
--
-- Valid Values:0, 300 to 2,592,000 seconds (5 minutes to 30 days)
cacheAttributes_cacheStaleTimeoutInSeconds :: Lens.Lens' CacheAttributes (Prelude.Maybe Prelude.Int)
cacheAttributes_cacheStaleTimeoutInSeconds = Lens.lens (\CacheAttributes' {cacheStaleTimeoutInSeconds} -> cacheStaleTimeoutInSeconds) (\s@CacheAttributes' {} a -> s {cacheStaleTimeoutInSeconds = a} :: CacheAttributes)

instance Data.FromJSON CacheAttributes where
  parseJSON =
    Data.withObject
      "CacheAttributes"
      ( \x ->
          CacheAttributes'
            Prelude.<$> (x Data..:? "CacheStaleTimeoutInSeconds")
      )

instance Prelude.Hashable CacheAttributes where
  hashWithSalt _salt CacheAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` cacheStaleTimeoutInSeconds

instance Prelude.NFData CacheAttributes where
  rnf CacheAttributes' {..} =
    Prelude.rnf cacheStaleTimeoutInSeconds

instance Data.ToJSON CacheAttributes where
  toJSON CacheAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CacheStaleTimeoutInSeconds" Data..=)
              Prelude.<$> cacheStaleTimeoutInSeconds
          ]
      )

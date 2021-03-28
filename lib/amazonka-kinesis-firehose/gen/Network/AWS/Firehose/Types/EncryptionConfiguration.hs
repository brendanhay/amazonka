{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.EncryptionConfiguration
  ( EncryptionConfiguration (..)
  -- * Smart constructor
  , mkEncryptionConfiguration
  -- * Lenses
  , ecKMSEncryptionConfig
  , ecNoEncryptionConfig
  ) where

import qualified Network.AWS.Firehose.Types.KMSEncryptionConfig as Types
import qualified Network.AWS.Firehose.Types.NoEncryptionConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the encryption for a destination in Amazon S3.
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { kMSEncryptionConfig :: Core.Maybe Types.KMSEncryptionConfig
    -- ^ The encryption key.
  , noEncryptionConfig :: Core.Maybe Types.NoEncryptionConfig
    -- ^ Specifically override existing encryption information to ensure that no encryption is used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionConfiguration' value with any optional fields omitted.
mkEncryptionConfiguration
    :: EncryptionConfiguration
mkEncryptionConfiguration
  = EncryptionConfiguration'{kMSEncryptionConfig = Core.Nothing,
                             noEncryptionConfig = Core.Nothing}

-- | The encryption key.
--
-- /Note:/ Consider using 'kMSEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKMSEncryptionConfig :: Lens.Lens' EncryptionConfiguration (Core.Maybe Types.KMSEncryptionConfig)
ecKMSEncryptionConfig = Lens.field @"kMSEncryptionConfig"
{-# INLINEABLE ecKMSEncryptionConfig #-}
{-# DEPRECATED kMSEncryptionConfig "Use generic-lens or generic-optics with 'kMSEncryptionConfig' instead"  #-}

-- | Specifically override existing encryption information to ensure that no encryption is used.
--
-- /Note:/ Consider using 'noEncryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecNoEncryptionConfig :: Lens.Lens' EncryptionConfiguration (Core.Maybe Types.NoEncryptionConfig)
ecNoEncryptionConfig = Lens.field @"noEncryptionConfig"
{-# INLINEABLE ecNoEncryptionConfig #-}
{-# DEPRECATED noEncryptionConfig "Use generic-lens or generic-optics with 'noEncryptionConfig' instead"  #-}

instance Core.FromJSON EncryptionConfiguration where
        toJSON EncryptionConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("KMSEncryptionConfig" Core..=) Core.<$> kMSEncryptionConfig,
                  ("NoEncryptionConfig" Core..=) Core.<$> noEncryptionConfig])

instance Core.FromJSON EncryptionConfiguration where
        parseJSON
          = Core.withObject "EncryptionConfiguration" Core.$
              \ x ->
                EncryptionConfiguration' Core.<$>
                  (x Core..:? "KMSEncryptionConfig") Core.<*>
                    x Core..:? "NoEncryptionConfig"

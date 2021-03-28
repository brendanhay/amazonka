{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
  ( EncryptionAtRestOptionsStatus (..)
  -- * Smart constructor
  , mkEncryptionAtRestOptionsStatus
  -- * Lenses
  , earosOptions
  , earosStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the Encryption At Rest options for the specified Elasticsearch domain.
--
-- /See:/ 'mkEncryptionAtRestOptionsStatus' smart constructor.
data EncryptionAtRestOptionsStatus = EncryptionAtRestOptionsStatus'
  { options :: Types.EncryptionAtRestOptions
    -- ^ Specifies the Encryption At Rest options for the specified Elasticsearch domain.
  , status :: Types.OptionStatus
    -- ^ Specifies the status of the Encryption At Rest options for the specified Elasticsearch domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EncryptionAtRestOptionsStatus' value with any optional fields omitted.
mkEncryptionAtRestOptionsStatus
    :: Types.EncryptionAtRestOptions -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> EncryptionAtRestOptionsStatus
mkEncryptionAtRestOptionsStatus options status
  = EncryptionAtRestOptionsStatus'{options, status}

-- | Specifies the Encryption At Rest options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earosOptions :: Lens.Lens' EncryptionAtRestOptionsStatus Types.EncryptionAtRestOptions
earosOptions = Lens.field @"options"
{-# INLINEABLE earosOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Specifies the status of the Encryption At Rest options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earosStatus :: Lens.Lens' EncryptionAtRestOptionsStatus Types.OptionStatus
earosStatus = Lens.field @"status"
{-# INLINEABLE earosStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON EncryptionAtRestOptionsStatus where
        parseJSON
          = Core.withObject "EncryptionAtRestOptionsStatus" Core.$
              \ x ->
                EncryptionAtRestOptionsStatus' Core.<$>
                  (x Core..: "Options") Core.<*> x Core..: "Status"

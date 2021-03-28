{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClassAnalysis
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.StorageClassAnalysis
  ( StorageClassAnalysis (..)
  -- * Smart constructor
  , mkStorageClassAnalysis
  -- * Lenses
  , scaDataExport
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.StorageClassAnalysisDataExport as Types

-- | Specifies data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes for an Amazon S3 bucket.
--
-- /See:/ 'mkStorageClassAnalysis' smart constructor.
newtype StorageClassAnalysis = StorageClassAnalysis'
  { dataExport :: Core.Maybe Types.StorageClassAnalysisDataExport
    -- ^ Specifies how data related to the storage class analysis for an Amazon S3 bucket should be exported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StorageClassAnalysis' value with any optional fields omitted.
mkStorageClassAnalysis
    :: StorageClassAnalysis
mkStorageClassAnalysis
  = StorageClassAnalysis'{dataExport = Core.Nothing}

-- | Specifies how data related to the storage class analysis for an Amazon S3 bucket should be exported.
--
-- /Note:/ Consider using 'dataExport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scaDataExport :: Lens.Lens' StorageClassAnalysis (Core.Maybe Types.StorageClassAnalysisDataExport)
scaDataExport = Lens.field @"dataExport"
{-# INLINEABLE scaDataExport #-}
{-# DEPRECATED dataExport "Use generic-lens or generic-optics with 'dataExport' instead"  #-}

instance Core.ToXML StorageClassAnalysis where
        toXML StorageClassAnalysis{..}
          = Core.maybe Core.mempty (Core.toXMLElement "DataExport")
              dataExport

instance Core.FromXML StorageClassAnalysis where
        parseXML x
          = StorageClassAnalysis' Core.<$> (x Core..@? "DataExport")

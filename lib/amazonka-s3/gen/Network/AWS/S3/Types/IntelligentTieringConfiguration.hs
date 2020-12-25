{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringConfiguration
  ( IntelligentTieringConfiguration (..),

    -- * Smart constructor
    mkIntelligentTieringConfiguration,

    -- * Lenses
    itcId,
    itcStatus,
    itcTierings,
    itcFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Id as Types
import qualified Network.AWS.S3.Types.IntelligentTieringFilter as Types
import qualified Network.AWS.S3.Types.IntelligentTieringStatus as Types
import qualified Network.AWS.S3.Types.Tiering as Types

-- | Specifies the S3 Intelligent-Tiering configuration for an Amazon S3 bucket.
--
-- For information about the S3 Intelligent-Tiering storage class, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
--
-- /See:/ 'mkIntelligentTieringConfiguration' smart constructor.
data IntelligentTieringConfiguration = IntelligentTieringConfiguration'
  { -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Types.Id,
    -- | Specifies the status of the configuration.
    status :: Types.IntelligentTieringStatus,
    -- | Specifies the S3 Intelligent-Tiering storage class tier of the configuration.
    tierings :: [Types.Tiering],
    -- | Specifies a bucket filter. The configuration only includes objects that meet the filter's criteria.
    filter :: Core.Maybe Types.IntelligentTieringFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntelligentTieringConfiguration' value with any optional fields omitted.
mkIntelligentTieringConfiguration ::
  -- | 'id'
  Types.Id ->
  -- | 'status'
  Types.IntelligentTieringStatus ->
  IntelligentTieringConfiguration
mkIntelligentTieringConfiguration id status =
  IntelligentTieringConfiguration'
    { id,
      status,
      tierings = Core.mempty,
      filter = Core.Nothing
    }

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcId :: Lens.Lens' IntelligentTieringConfiguration Types.Id
itcId = Lens.field @"id"
{-# DEPRECATED itcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies the status of the configuration.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcStatus :: Lens.Lens' IntelligentTieringConfiguration Types.IntelligentTieringStatus
itcStatus = Lens.field @"status"
{-# DEPRECATED itcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the S3 Intelligent-Tiering storage class tier of the configuration.
--
-- /Note:/ Consider using 'tierings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcTierings :: Lens.Lens' IntelligentTieringConfiguration [Types.Tiering]
itcTierings = Lens.field @"tierings"
{-# DEPRECATED itcTierings "Use generic-lens or generic-optics with 'tierings' instead." #-}

-- | Specifies a bucket filter. The configuration only includes objects that meet the filter's criteria.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itcFilter :: Lens.Lens' IntelligentTieringConfiguration (Core.Maybe Types.IntelligentTieringFilter)
itcFilter = Lens.field @"filter"
{-# DEPRECATED itcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Core.ToXML IntelligentTieringConfiguration where
  toXML IntelligentTieringConfiguration {..} =
    Core.toXMLNode "Id" id Core.<> Core.toXMLNode "Status" status
      Core.<> Core.toXMLList "Tiering" tierings
      Core.<> Core.toXMLNode "Filter" Core.<$> filter

instance Core.FromXML IntelligentTieringConfiguration where
  parseXML x =
    IntelligentTieringConfiguration'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@? "Tiering" Core..@! Core.mempty)
      Core.<*> (x Core..@? "Filter")

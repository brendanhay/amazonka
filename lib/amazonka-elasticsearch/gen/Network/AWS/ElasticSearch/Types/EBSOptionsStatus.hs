{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EBSOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EBSOptionsStatus
  ( EBSOptionsStatus (..),

    -- * Smart constructor
    mkEBSOptionsStatus,

    -- * Lenses
    ebsosOptions,
    ebsosStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types.EBSOptions as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the EBS options for the specified Elasticsearch domain.
--
-- /See:/ 'mkEBSOptionsStatus' smart constructor.
data EBSOptionsStatus = EBSOptionsStatus'
  { -- | Specifies the EBS options for the specified Elasticsearch domain.
    options :: Types.EBSOptions,
    -- | Specifies the status of the EBS options for the specified Elasticsearch domain.
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EBSOptionsStatus' value with any optional fields omitted.
mkEBSOptionsStatus ::
  -- | 'options'
  Types.EBSOptions ->
  -- | 'status'
  Types.OptionStatus ->
  EBSOptionsStatus
mkEBSOptionsStatus options status =
  EBSOptionsStatus' {options, status}

-- | Specifies the EBS options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsosOptions :: Lens.Lens' EBSOptionsStatus Types.EBSOptions
ebsosOptions = Lens.field @"options"
{-# DEPRECATED ebsosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Specifies the status of the EBS options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsosStatus :: Lens.Lens' EBSOptionsStatus Types.OptionStatus
ebsosStatus = Lens.field @"status"
{-# DEPRECATED ebsosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON EBSOptionsStatus where
  parseJSON =
    Core.withObject "EBSOptionsStatus" Core.$
      \x ->
        EBSOptionsStatus'
          Core.<$> (x Core..: "Options") Core.<*> (x Core..: "Status")

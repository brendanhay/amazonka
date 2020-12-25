{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
  ( AdvancedSecurityOptionsStatus (..),

    -- * Smart constructor
    mkAdvancedSecurityOptionsStatus,

    -- * Lenses
    asosOptions,
    asosStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status of advanced security options for the specified Elasticsearch domain.
--
-- /See:/ 'mkAdvancedSecurityOptionsStatus' smart constructor.
data AdvancedSecurityOptionsStatus = AdvancedSecurityOptionsStatus'
  { -- | Specifies advanced security options for the specified Elasticsearch domain.
    options :: Types.AdvancedSecurityOptions,
    -- | Status of the advanced security options for the specified Elasticsearch domain.
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AdvancedSecurityOptionsStatus' value with any optional fields omitted.
mkAdvancedSecurityOptionsStatus ::
  -- | 'options'
  Types.AdvancedSecurityOptions ->
  -- | 'status'
  Types.OptionStatus ->
  AdvancedSecurityOptionsStatus
mkAdvancedSecurityOptionsStatus options status =
  AdvancedSecurityOptionsStatus' {options, status}

-- | Specifies advanced security options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asosOptions :: Lens.Lens' AdvancedSecurityOptionsStatus Types.AdvancedSecurityOptions
asosOptions = Lens.field @"options"
{-# DEPRECATED asosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Status of the advanced security options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asosStatus :: Lens.Lens' AdvancedSecurityOptionsStatus Types.OptionStatus
asosStatus = Lens.field @"status"
{-# DEPRECATED asosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON AdvancedSecurityOptionsStatus where
  parseJSON =
    Core.withObject "AdvancedSecurityOptionsStatus" Core.$
      \x ->
        AdvancedSecurityOptionsStatus'
          Core.<$> (x Core..: "Options") Core.<*> (x Core..: "Status")

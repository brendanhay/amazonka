{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.TemplateMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.TemplateMetadata
  ( TemplateMetadata (..)
  -- * Smart constructor
  , mkTemplateMetadata
  -- * Lenses
  , tmCreatedTimestamp
  , tmName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.TemplateName as Types

-- | Contains information about an email template.
--
-- /See:/ 'mkTemplateMetadata' smart constructor.
data TemplateMetadata = TemplateMetadata'
  { createdTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The time and date the template was created.
  , name :: Core.Maybe Types.TemplateName
    -- ^ The name of the template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TemplateMetadata' value with any optional fields omitted.
mkTemplateMetadata
    :: TemplateMetadata
mkTemplateMetadata
  = TemplateMetadata'{createdTimestamp = Core.Nothing,
                      name = Core.Nothing}

-- | The time and date the template was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmCreatedTimestamp :: Lens.Lens' TemplateMetadata (Core.Maybe Core.UTCTime)
tmCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE tmCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The name of the template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmName :: Lens.Lens' TemplateMetadata (Core.Maybe Types.TemplateName)
tmName = Lens.field @"name"
{-# INLINEABLE tmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML TemplateMetadata where
        parseXML x
          = TemplateMetadata' Core.<$>
              (x Core..@? "CreatedTimestamp") Core.<*> x Core..@? "Name"

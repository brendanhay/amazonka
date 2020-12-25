{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
  ( SourceConfiguration (..),

    -- * Smart constructor
    mkSourceConfiguration,

    -- * Lenses
    scApplicationName,
    scTemplateName,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationTemplateName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A specification for an environment configuration.
--
-- /See:/ 'mkSourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { -- | The name of the application associated with the configuration.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The name of the configuration template.
    templateName :: Core.Maybe Types.ConfigurationTemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceConfiguration' value with any optional fields omitted.
mkSourceConfiguration ::
  SourceConfiguration
mkSourceConfiguration =
  SourceConfiguration'
    { applicationName = Core.Nothing,
      templateName = Core.Nothing
    }

-- | The name of the application associated with the configuration.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scApplicationName :: Lens.Lens' SourceConfiguration (Core.Maybe Types.ApplicationName)
scApplicationName = Lens.field @"applicationName"
{-# DEPRECATED scApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the configuration template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTemplateName :: Lens.Lens' SourceConfiguration (Core.Maybe Types.ConfigurationTemplateName)
scTemplateName = Lens.field @"templateName"
{-# DEPRECATED scTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

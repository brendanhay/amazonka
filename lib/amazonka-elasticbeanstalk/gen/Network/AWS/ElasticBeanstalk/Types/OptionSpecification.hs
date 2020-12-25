{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.OptionSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.OptionSpecification
  ( OptionSpecification (..),

    -- * Smart constructor
    mkOptionSpecification,

    -- * Lenses
    osNamespace,
    osOptionName,
    osResourceName,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.OptionName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.OptionNamespace as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ResourceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A specification identifying an individual configuration option.
--
-- /See:/ 'mkOptionSpecification' smart constructor.
data OptionSpecification = OptionSpecification'
  { -- | A unique namespace identifying the option's associated AWS resource.
    namespace :: Core.Maybe Types.OptionNamespace,
    -- | The name of the configuration option.
    optionName :: Core.Maybe Types.OptionName,
    -- | A unique resource name for a time-based scaling configuration option.
    resourceName :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionSpecification' value with any optional fields omitted.
mkOptionSpecification ::
  OptionSpecification
mkOptionSpecification =
  OptionSpecification'
    { namespace = Core.Nothing,
      optionName = Core.Nothing,
      resourceName = Core.Nothing
    }

-- | A unique namespace identifying the option's associated AWS resource.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osNamespace :: Lens.Lens' OptionSpecification (Core.Maybe Types.OptionNamespace)
osNamespace = Lens.field @"namespace"
{-# DEPRECATED osNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The name of the configuration option.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOptionName :: Lens.Lens' OptionSpecification (Core.Maybe Types.OptionName)
osOptionName = Lens.field @"optionName"
{-# DEPRECATED osOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

-- | A unique resource name for a time-based scaling configuration option.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osResourceName :: Lens.Lens' OptionSpecification (Core.Maybe Types.ResourceName)
osResourceName = Lens.field @"resourceName"
{-# DEPRECATED osResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

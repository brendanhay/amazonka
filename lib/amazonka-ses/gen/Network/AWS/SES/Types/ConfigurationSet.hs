{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ConfigurationSet
  ( ConfigurationSet (..),

    -- * Smart constructor
    mkConfigurationSet,

    -- * Lenses
    csName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.ConfigurationSetName as Types

-- | The name of the configuration set.
--
-- Configuration sets let you create groups of rules that you can apply to the emails you send using Amazon SES. For more information about using configuration sets, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-configuration-sets.html Using Amazon SES Configuration Sets> in the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/ Amazon SES Developer Guide> .
--
-- /See:/ 'mkConfigurationSet' smart constructor.
newtype ConfigurationSet = ConfigurationSet'
  { -- | The name of the configuration set. The name must meet the following requirements:
    --
    --
    --     * Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
    --
    --
    --     * Contain 64 characters or fewer.
    name :: Types.ConfigurationSetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigurationSet' value with any optional fields omitted.
mkConfigurationSet ::
  -- | 'name'
  Types.ConfigurationSetName ->
  ConfigurationSet
mkConfigurationSet name = ConfigurationSet' {name}

-- | The name of the configuration set. The name must meet the following requirements:
--
--
--     * Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain 64 characters or fewer.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ConfigurationSet Types.ConfigurationSetName
csName = Lens.field @"name"
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromXML ConfigurationSet where
  parseXML x = ConfigurationSet' Core.<$> (x Core..@ "Name")

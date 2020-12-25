{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestinationConfiguration
  ( TopicRuleDestinationConfiguration (..),

    -- * Smart constructor
    mkTopicRuleDestinationConfiguration,

    -- * Lenses
    trdcHttpUrlConfiguration,
  )
where

import qualified Network.AWS.IoT.Types.HttpUrlDestinationConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration of the topic rule destination.
--
-- /See:/ 'mkTopicRuleDestinationConfiguration' smart constructor.
newtype TopicRuleDestinationConfiguration = TopicRuleDestinationConfiguration'
  { -- | Configuration of the HTTP URL.
    httpUrlConfiguration :: Core.Maybe Types.HttpUrlDestinationConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TopicRuleDestinationConfiguration' value with any optional fields omitted.
mkTopicRuleDestinationConfiguration ::
  TopicRuleDestinationConfiguration
mkTopicRuleDestinationConfiguration =
  TopicRuleDestinationConfiguration'
    { httpUrlConfiguration =
        Core.Nothing
    }

-- | Configuration of the HTTP URL.
--
-- /Note:/ Consider using 'httpUrlConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdcHttpUrlConfiguration :: Lens.Lens' TopicRuleDestinationConfiguration (Core.Maybe Types.HttpUrlDestinationConfiguration)
trdcHttpUrlConfiguration = Lens.field @"httpUrlConfiguration"
{-# DEPRECATED trdcHttpUrlConfiguration "Use generic-lens or generic-optics with 'httpUrlConfiguration' instead." #-}

instance Core.FromJSON TopicRuleDestinationConfiguration where
  toJSON TopicRuleDestinationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [("httpUrlConfiguration" Core..=) Core.<$> httpUrlConfiguration]
      )

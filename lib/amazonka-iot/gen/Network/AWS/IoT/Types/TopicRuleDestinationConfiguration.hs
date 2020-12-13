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
    trdcHttpURLConfiguration,
  )
where

import Network.AWS.IoT.Types.HTTPURLDestinationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration of the topic rule destination.
--
-- /See:/ 'mkTopicRuleDestinationConfiguration' smart constructor.
newtype TopicRuleDestinationConfiguration = TopicRuleDestinationConfiguration'
  { -- | Configuration of the HTTP URL.
    httpURLConfiguration :: Lude.Maybe HTTPURLDestinationConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TopicRuleDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'httpURLConfiguration' - Configuration of the HTTP URL.
mkTopicRuleDestinationConfiguration ::
  TopicRuleDestinationConfiguration
mkTopicRuleDestinationConfiguration =
  TopicRuleDestinationConfiguration'
    { httpURLConfiguration =
        Lude.Nothing
    }

-- | Configuration of the HTTP URL.
--
-- /Note:/ Consider using 'httpURLConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdcHttpURLConfiguration :: Lens.Lens' TopicRuleDestinationConfiguration (Lude.Maybe HTTPURLDestinationConfiguration)
trdcHttpURLConfiguration = Lens.lens (httpURLConfiguration :: TopicRuleDestinationConfiguration -> Lude.Maybe HTTPURLDestinationConfiguration) (\s a -> s {httpURLConfiguration = a} :: TopicRuleDestinationConfiguration)
{-# DEPRECATED trdcHttpURLConfiguration "Use generic-lens or generic-optics with 'httpURLConfiguration' instead." #-}

instance Lude.ToJSON TopicRuleDestinationConfiguration where
  toJSON TopicRuleDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("httpUrlConfiguration" Lude..=) Lude.<$> httpURLConfiguration]
      )

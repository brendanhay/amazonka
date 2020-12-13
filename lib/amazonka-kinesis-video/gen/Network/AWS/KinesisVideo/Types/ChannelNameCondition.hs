{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ChannelNameCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ChannelNameCondition
  ( ChannelNameCondition (..),

    -- * Smart constructor
    mkChannelNameCondition,

    -- * Lenses
    cncComparisonOperator,
    cncComparisonValue,
  )
where

import Network.AWS.KinesisVideo.Types.ComparisonOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An optional input parameter for the @ListSignalingChannels@ API. When this parameter is specified while invoking @ListSignalingChannels@ , the API returns only the channels that satisfy a condition specified in @ChannelNameCondition@ .
--
-- /See:/ 'mkChannelNameCondition' smart constructor.
data ChannelNameCondition = ChannelNameCondition'
  { -- | A comparison operator. Currently, you can only specify the @BEGINS_WITH@ operator, which finds signaling channels whose names begin with a given prefix.
    comparisonOperator :: Lude.Maybe ComparisonOperator,
    -- | A value to compare.
    comparisonValue :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelNameCondition' with the minimum fields required to make a request.
--
-- * 'comparisonOperator' - A comparison operator. Currently, you can only specify the @BEGINS_WITH@ operator, which finds signaling channels whose names begin with a given prefix.
-- * 'comparisonValue' - A value to compare.
mkChannelNameCondition ::
  ChannelNameCondition
mkChannelNameCondition =
  ChannelNameCondition'
    { comparisonOperator = Lude.Nothing,
      comparisonValue = Lude.Nothing
    }

-- | A comparison operator. Currently, you can only specify the @BEGINS_WITH@ operator, which finds signaling channels whose names begin with a given prefix.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cncComparisonOperator :: Lens.Lens' ChannelNameCondition (Lude.Maybe ComparisonOperator)
cncComparisonOperator = Lens.lens (comparisonOperator :: ChannelNameCondition -> Lude.Maybe ComparisonOperator) (\s a -> s {comparisonOperator = a} :: ChannelNameCondition)
{-# DEPRECATED cncComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | A value to compare.
--
-- /Note:/ Consider using 'comparisonValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cncComparisonValue :: Lens.Lens' ChannelNameCondition (Lude.Maybe Lude.Text)
cncComparisonValue = Lens.lens (comparisonValue :: ChannelNameCondition -> Lude.Maybe Lude.Text) (\s a -> s {comparisonValue = a} :: ChannelNameCondition)
{-# DEPRECATED cncComparisonValue "Use generic-lens or generic-optics with 'comparisonValue' instead." #-}

instance Lude.ToJSON ChannelNameCondition where
  toJSON ChannelNameCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ComparisonOperator" Lude..=) Lude.<$> comparisonOperator,
            ("ComparisonValue" Lude..=) Lude.<$> comparisonValue
          ]
      )

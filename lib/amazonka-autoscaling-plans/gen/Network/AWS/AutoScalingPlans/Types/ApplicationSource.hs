{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ApplicationSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ApplicationSource
  ( ApplicationSource (..),

    -- * Smart constructor
    mkApplicationSource,

    -- * Lenses
    asTagFilters,
    asCloudFormationStackARN,
  )
where

import Network.AWS.AutoScalingPlans.Types.TagFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an application source.
--
-- /See:/ 'mkApplicationSource' smart constructor.
data ApplicationSource = ApplicationSource'
  { tagFilters ::
      Lude.Maybe [TagFilter],
    cloudFormationStackARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationSource' with the minimum fields required to make a request.
--
-- * 'cloudFormationStackARN' - The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
-- * 'tagFilters' - A set of tags (up to 50).
mkApplicationSource ::
  ApplicationSource
mkApplicationSource =
  ApplicationSource'
    { tagFilters = Lude.Nothing,
      cloudFormationStackARN = Lude.Nothing
    }

-- | A set of tags (up to 50).
--
-- /Note:/ Consider using 'tagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTagFilters :: Lens.Lens' ApplicationSource (Lude.Maybe [TagFilter])
asTagFilters = Lens.lens (tagFilters :: ApplicationSource -> Lude.Maybe [TagFilter]) (\s a -> s {tagFilters = a} :: ApplicationSource)
{-# DEPRECATED asTagFilters "Use generic-lens or generic-optics with 'tagFilters' instead." #-}

-- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
--
-- /Note:/ Consider using 'cloudFormationStackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCloudFormationStackARN :: Lens.Lens' ApplicationSource (Lude.Maybe Lude.Text)
asCloudFormationStackARN = Lens.lens (cloudFormationStackARN :: ApplicationSource -> Lude.Maybe Lude.Text) (\s a -> s {cloudFormationStackARN = a} :: ApplicationSource)
{-# DEPRECATED asCloudFormationStackARN "Use generic-lens or generic-optics with 'cloudFormationStackARN' instead." #-}

instance Lude.FromJSON ApplicationSource where
  parseJSON =
    Lude.withObject
      "ApplicationSource"
      ( \x ->
          ApplicationSource'
            Lude.<$> (x Lude..:? "TagFilters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CloudFormationStackARN")
      )

instance Lude.ToJSON ApplicationSource where
  toJSON ApplicationSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TagFilters" Lude..=) Lude.<$> tagFilters,
            ("CloudFormationStackARN" Lude..=)
              Lude.<$> cloudFormationStackARN
          ]
      )

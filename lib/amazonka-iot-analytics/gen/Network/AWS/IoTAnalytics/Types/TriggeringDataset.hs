{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.TriggeringDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.TriggeringDataset
  ( TriggeringDataset (..),

    -- * Smart constructor
    mkTriggeringDataset,

    -- * Lenses
    tdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the dataset whose content generation triggers the new dataset content generation.
--
-- /See:/ 'mkTriggeringDataset' smart constructor.
newtype TriggeringDataset = TriggeringDataset'
  { -- | The name of the dataset whose content generation triggers the new dataset content generation.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TriggeringDataset' with the minimum fields required to make a request.
--
-- * 'name' - The name of the dataset whose content generation triggers the new dataset content generation.
mkTriggeringDataset ::
  -- | 'name'
  Lude.Text ->
  TriggeringDataset
mkTriggeringDataset pName_ = TriggeringDataset' {name = pName_}

-- | The name of the dataset whose content generation triggers the new dataset content generation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdName :: Lens.Lens' TriggeringDataset Lude.Text
tdName = Lens.lens (name :: TriggeringDataset -> Lude.Text) (\s a -> s {name = a} :: TriggeringDataset)
{-# DEPRECATED tdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON TriggeringDataset where
  parseJSON =
    Lude.withObject
      "TriggeringDataset"
      (\x -> TriggeringDataset' Lude.<$> (x Lude..: "name"))

instance Lude.ToJSON TriggeringDataset where
  toJSON TriggeringDataset' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

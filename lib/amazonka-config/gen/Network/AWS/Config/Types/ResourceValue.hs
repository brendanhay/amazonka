{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceValue
  ( ResourceValue (..),

    -- * Smart constructor
    mkResourceValue,

    -- * Lenses
    rvValue,
  )
where

import Network.AWS.Config.Types.ResourceValueType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The dynamic value of the resource.
--
-- /See:/ 'mkResourceValue' smart constructor.
newtype ResourceValue = ResourceValue'
  { -- | The value is a resource ID.
    value :: ResourceValueType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceValue' with the minimum fields required to make a request.
--
-- * 'value' - The value is a resource ID.
mkResourceValue ::
  -- | 'value'
  ResourceValueType ->
  ResourceValue
mkResourceValue pValue_ = ResourceValue' {value = pValue_}

-- | The value is a resource ID.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvValue :: Lens.Lens' ResourceValue ResourceValueType
rvValue = Lens.lens (value :: ResourceValue -> ResourceValueType) (\s a -> s {value = a} :: ResourceValue)
{-# DEPRECATED rvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON ResourceValue where
  parseJSON =
    Lude.withObject
      "ResourceValue"
      (\x -> ResourceValue' Lude.<$> (x Lude..: "Value"))

instance Lude.ToJSON ResourceValue where
  toJSON ResourceValue' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Value" Lude..= value)])

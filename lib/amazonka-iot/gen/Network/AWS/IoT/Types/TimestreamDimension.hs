{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamDimension
  ( TimestreamDimension (..),

    -- * Smart constructor
    mkTimestreamDimension,

    -- * Lenses
    tdName,
    tdValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Metadata attributes of the time series that are written in each measure record.
--
-- /See:/ 'mkTimestreamDimension' smart constructor.
data TimestreamDimension = TimestreamDimension'
  { name :: Lude.Text,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimestreamDimension' with the minimum fields required to make a request.
--
-- * 'name' - The metadata dimension name. This is the name of the column in the Amazon Timestream database table record.
--
-- Dimensions cannot be named: @measure_name@ , @measure_value@ , or @time@ . These names are reserved. Dimension names cannot start with @ts_@ or @measure_value@ and they cannot contain the colon (@:@ ) character.
-- * 'value' - The value to write in this column of the database record.
mkTimestreamDimension ::
  -- | 'name'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  TimestreamDimension
mkTimestreamDimension pName_ pValue_ =
  TimestreamDimension' {name = pName_, value = pValue_}

-- | The metadata dimension name. This is the name of the column in the Amazon Timestream database table record.
--
-- Dimensions cannot be named: @measure_name@ , @measure_value@ , or @time@ . These names are reserved. Dimension names cannot start with @ts_@ or @measure_value@ and they cannot contain the colon (@:@ ) character.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdName :: Lens.Lens' TimestreamDimension Lude.Text
tdName = Lens.lens (name :: TimestreamDimension -> Lude.Text) (\s a -> s {name = a} :: TimestreamDimension)
{-# DEPRECATED tdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value to write in this column of the database record.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdValue :: Lens.Lens' TimestreamDimension Lude.Text
tdValue = Lens.lens (value :: TimestreamDimension -> Lude.Text) (\s a -> s {value = a} :: TimestreamDimension)
{-# DEPRECATED tdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON TimestreamDimension where
  parseJSON =
    Lude.withObject
      "TimestreamDimension"
      ( \x ->
          TimestreamDimension'
            Lude.<$> (x Lude..: "name") Lude.<*> (x Lude..: "value")
      )

instance Lude.ToJSON TimestreamDimension where
  toJSON TimestreamDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("value" Lude..= value)
          ]
      )

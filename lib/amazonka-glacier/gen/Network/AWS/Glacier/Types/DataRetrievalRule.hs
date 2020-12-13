{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.DataRetrievalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.DataRetrievalRule
  ( DataRetrievalRule (..),

    -- * Smart constructor
    mkDataRetrievalRule,

    -- * Lenses
    drrStrategy,
    drrBytesPerHour,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Data retrieval policy rule.
--
-- /See:/ 'mkDataRetrievalRule' smart constructor.
data DataRetrievalRule = DataRetrievalRule'
  { -- | The type of data retrieval policy to set.
    --
    -- Valid values: BytesPerHour|FreeTier|None
    strategy :: Lude.Maybe Lude.Text,
    -- | The maximum number of bytes that can be retrieved in an hour.
    --
    -- This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
    bytesPerHour :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataRetrievalRule' with the minimum fields required to make a request.
--
-- * 'strategy' - The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
-- * 'bytesPerHour' - The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
mkDataRetrievalRule ::
  DataRetrievalRule
mkDataRetrievalRule =
  DataRetrievalRule'
    { strategy = Lude.Nothing,
      bytesPerHour = Lude.Nothing
    }

-- | The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrStrategy :: Lens.Lens' DataRetrievalRule (Lude.Maybe Lude.Text)
drrStrategy = Lens.lens (strategy :: DataRetrievalRule -> Lude.Maybe Lude.Text) (\s a -> s {strategy = a} :: DataRetrievalRule)
{-# DEPRECATED drrStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

-- | The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
--
-- /Note:/ Consider using 'bytesPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrBytesPerHour :: Lens.Lens' DataRetrievalRule (Lude.Maybe Lude.Integer)
drrBytesPerHour = Lens.lens (bytesPerHour :: DataRetrievalRule -> Lude.Maybe Lude.Integer) (\s a -> s {bytesPerHour = a} :: DataRetrievalRule)
{-# DEPRECATED drrBytesPerHour "Use generic-lens or generic-optics with 'bytesPerHour' instead." #-}

instance Lude.FromJSON DataRetrievalRule where
  parseJSON =
    Lude.withObject
      "DataRetrievalRule"
      ( \x ->
          DataRetrievalRule'
            Lude.<$> (x Lude..:? "Strategy") Lude.<*> (x Lude..:? "BytesPerHour")
      )

instance Lude.ToJSON DataRetrievalRule where
  toJSON DataRetrievalRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Strategy" Lude..=) Lude.<$> strategy,
            ("BytesPerHour" Lude..=) Lude.<$> bytesPerHour
          ]
      )

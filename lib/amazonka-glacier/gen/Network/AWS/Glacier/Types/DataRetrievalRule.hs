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
    drrBytesPerHour,
    drrStrategy,
  )
where

import qualified Network.AWS.Glacier.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Data retrieval policy rule.
--
-- /See:/ 'mkDataRetrievalRule' smart constructor.
data DataRetrievalRule = DataRetrievalRule'
  { -- | The maximum number of bytes that can be retrieved in an hour.
    --
    -- This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
    bytesPerHour :: Core.Maybe Core.Integer,
    -- | The type of data retrieval policy to set.
    --
    -- Valid values: BytesPerHour|FreeTier|None
    strategy :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataRetrievalRule' value with any optional fields omitted.
mkDataRetrievalRule ::
  DataRetrievalRule
mkDataRetrievalRule =
  DataRetrievalRule'
    { bytesPerHour = Core.Nothing,
      strategy = Core.Nothing
    }

-- | The maximum number of bytes that can be retrieved in an hour.
--
-- This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
--
-- /Note:/ Consider using 'bytesPerHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrBytesPerHour :: Lens.Lens' DataRetrievalRule (Core.Maybe Core.Integer)
drrBytesPerHour = Lens.field @"bytesPerHour"
{-# DEPRECATED drrBytesPerHour "Use generic-lens or generic-optics with 'bytesPerHour' instead." #-}

-- | The type of data retrieval policy to set.
--
-- Valid values: BytesPerHour|FreeTier|None
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrStrategy :: Lens.Lens' DataRetrievalRule (Core.Maybe Types.String)
drrStrategy = Lens.field @"strategy"
{-# DEPRECATED drrStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

instance Core.FromJSON DataRetrievalRule where
  toJSON DataRetrievalRule {..} =
    Core.object
      ( Core.catMaybes
          [ ("BytesPerHour" Core..=) Core.<$> bytesPerHour,
            ("Strategy" Core..=) Core.<$> strategy
          ]
      )

instance Core.FromJSON DataRetrievalRule where
  parseJSON =
    Core.withObject "DataRetrievalRule" Core.$
      \x ->
        DataRetrievalRule'
          Core.<$> (x Core..:? "BytesPerHour") Core.<*> (x Core..:? "Strategy")

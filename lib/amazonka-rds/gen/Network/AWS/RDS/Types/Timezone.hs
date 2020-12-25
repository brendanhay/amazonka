{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Timezone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Timezone
  ( Timezone (..),

    -- * Smart constructor
    mkTimezone,

    -- * Lenses
    tTimezoneName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | A time zone associated with a @DBInstance@ or a @DBSnapshot@ . This data type is an element in the response to the @DescribeDBInstances@ , the @DescribeDBSnapshots@ , and the @DescribeDBEngineVersions@ actions.
--
-- /See:/ 'mkTimezone' smart constructor.
newtype Timezone = Timezone'
  { -- | The name of the time zone.
    timezoneName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Timezone' value with any optional fields omitted.
mkTimezone ::
  Timezone
mkTimezone = Timezone' {timezoneName = Core.Nothing}

-- | The name of the time zone.
--
-- /Note:/ Consider using 'timezoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTimezoneName :: Lens.Lens' Timezone (Core.Maybe Types.String)
tTimezoneName = Lens.field @"timezoneName"
{-# DEPRECATED tTimezoneName "Use generic-lens or generic-optics with 'timezoneName' instead." #-}

instance Core.FromXML Timezone where
  parseXML x = Timezone' Core.<$> (x Core..@? "TimezoneName")

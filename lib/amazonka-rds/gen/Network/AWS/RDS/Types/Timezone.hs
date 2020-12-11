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
import qualified Network.AWS.Prelude as Lude

-- | A time zone associated with a @DBInstance@ or a @DBSnapshot@ . This data type is an element in the response to the @DescribeDBInstances@ , the @DescribeDBSnapshots@ , and the @DescribeDBEngineVersions@ actions.
--
-- /See:/ 'mkTimezone' smart constructor.
newtype Timezone = Timezone' {timezoneName :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Timezone' with the minimum fields required to make a request.
--
-- * 'timezoneName' - The name of the time zone.
mkTimezone ::
  Timezone
mkTimezone = Timezone' {timezoneName = Lude.Nothing}

-- | The name of the time zone.
--
-- /Note:/ Consider using 'timezoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTimezoneName :: Lens.Lens' Timezone (Lude.Maybe Lude.Text)
tTimezoneName = Lens.lens (timezoneName :: Timezone -> Lude.Maybe Lude.Text) (\s a -> s {timezoneName = a} :: Timezone)
{-# DEPRECATED tTimezoneName "Use generic-lens or generic-optics with 'timezoneName' instead." #-}

instance Lude.FromXML Timezone where
  parseXML x = Timezone' Lude.<$> (x Lude..@? "TimezoneName")

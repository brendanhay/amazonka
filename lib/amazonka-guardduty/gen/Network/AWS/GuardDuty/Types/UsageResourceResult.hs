{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageResourceResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageResourceResult
  ( UsageResourceResult (..),

    -- * Smart constructor
    mkUsageResourceResult,

    -- * Lenses
    urrTotal,
    urrResource,
  )
where

import Network.AWS.GuardDuty.Types.Total
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the sum of usage based on an AWS resource.
--
-- /See:/ 'mkUsageResourceResult' smart constructor.
data UsageResourceResult = UsageResourceResult'
  { total ::
      Lude.Maybe Total,
    resource :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageResourceResult' with the minimum fields required to make a request.
--
-- * 'resource' - The AWS resource that generated usage.
-- * 'total' - Represents the sum total of usage for the specified resource type.
mkUsageResourceResult ::
  UsageResourceResult
mkUsageResourceResult =
  UsageResourceResult'
    { total = Lude.Nothing,
      resource = Lude.Nothing
    }

-- | Represents the sum total of usage for the specified resource type.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrTotal :: Lens.Lens' UsageResourceResult (Lude.Maybe Total)
urrTotal = Lens.lens (total :: UsageResourceResult -> Lude.Maybe Total) (\s a -> s {total = a} :: UsageResourceResult)
{-# DEPRECATED urrTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The AWS resource that generated usage.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrResource :: Lens.Lens' UsageResourceResult (Lude.Maybe Lude.Text)
urrResource = Lens.lens (resource :: UsageResourceResult -> Lude.Maybe Lude.Text) (\s a -> s {resource = a} :: UsageResourceResult)
{-# DEPRECATED urrResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON UsageResourceResult where
  parseJSON =
    Lude.withObject
      "UsageResourceResult"
      ( \x ->
          UsageResourceResult'
            Lude.<$> (x Lude..:? "total") Lude.<*> (x Lude..:? "resource")
      )

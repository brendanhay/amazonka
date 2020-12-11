-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AlarmIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.AlarmIdentifier
  ( AlarmIdentifier (..),

    -- * Smart constructor
    mkAlarmIdentifier,

    -- * Lenses
    aiRegion,
    aiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.CloudWatchRegion

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether the specified health check is healthy.
--
-- /See:/ 'mkAlarmIdentifier' smart constructor.
data AlarmIdentifier = AlarmIdentifier'
  { region :: CloudWatchRegion,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AlarmIdentifier' with the minimum fields required to make a request.
--
-- * 'name' - The name of the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
-- * 'region' - For the CloudWatch alarm that you want Route 53 health checkers to use to determine whether this health check is healthy, the region that the alarm was created in.
--
-- For the current list of CloudWatch regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#cw_region Amazon CloudWatch> in the /AWS Service Endpoints/ chapter of the /Amazon Web Services General Reference/ .
mkAlarmIdentifier ::
  -- | 'region'
  CloudWatchRegion ->
  -- | 'name'
  Lude.Text ->
  AlarmIdentifier
mkAlarmIdentifier pRegion_ pName_ =
  AlarmIdentifier' {region = pRegion_, name = pName_}

-- | For the CloudWatch alarm that you want Route 53 health checkers to use to determine whether this health check is healthy, the region that the alarm was created in.
--
-- For the current list of CloudWatch regions, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#cw_region Amazon CloudWatch> in the /AWS Service Endpoints/ chapter of the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiRegion :: Lens.Lens' AlarmIdentifier CloudWatchRegion
aiRegion = Lens.lens (region :: AlarmIdentifier -> CloudWatchRegion) (\s a -> s {region = a} :: AlarmIdentifier)
{-# DEPRECATED aiRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The name of the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiName :: Lens.Lens' AlarmIdentifier Lude.Text
aiName = Lens.lens (name :: AlarmIdentifier -> Lude.Text) (\s a -> s {name = a} :: AlarmIdentifier)
{-# DEPRECATED aiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML AlarmIdentifier where
  parseXML x =
    AlarmIdentifier'
      Lude.<$> (x Lude..@ "Region") Lude.<*> (x Lude..@ "Name")

instance Lude.ToXML AlarmIdentifier where
  toXML AlarmIdentifier' {..} =
    Lude.mconcat ["Region" Lude.@= region, "Name" Lude.@= name]

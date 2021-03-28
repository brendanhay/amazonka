{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
  ( DeltaTimeSessionWindowConfiguration (..)
  -- * Smart constructor
  , mkDeltaTimeSessionWindowConfiguration
  -- * Lenses
  , dtswcTimeoutInMinutes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that contains the configuration information of a delta time session window.
--
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html @DeltaTime@ > specifies a time interval. You can use @DeltaTime@ to create dataset contents with data that has arrived in the data store since the last execution. For an example of @DeltaTime@ , see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/automate-create-dataset.html#automate-example6 Creating a SQL dataset with a delta window (CLI)> in the /AWS IoT Analytics User Guide/ .
--
-- /See:/ 'mkDeltaTimeSessionWindowConfiguration' smart constructor.
newtype DeltaTimeSessionWindowConfiguration = DeltaTimeSessionWindowConfiguration'
  { timeoutInMinutes :: Core.Natural
    -- ^ A time interval. You can use @timeoutInMinutes@ so that AWS IoT Analytics can batch up late data notifications that have been generated since the last execution. AWS IoT Analytics sends one batch of notifications to Amazon CloudWatch Events at one time.
--
-- For more information about how to write a timestamp expression, see <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators> , in the /Presto 0.172 Documentation/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeltaTimeSessionWindowConfiguration' value with any optional fields omitted.
mkDeltaTimeSessionWindowConfiguration
    :: Core.Natural -- ^ 'timeoutInMinutes'
    -> DeltaTimeSessionWindowConfiguration
mkDeltaTimeSessionWindowConfiguration timeoutInMinutes
  = DeltaTimeSessionWindowConfiguration'{timeoutInMinutes}

-- | A time interval. You can use @timeoutInMinutes@ so that AWS IoT Analytics can batch up late data notifications that have been generated since the last execution. AWS IoT Analytics sends one batch of notifications to Amazon CloudWatch Events at one time.
--
-- For more information about how to write a timestamp expression, see <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators> , in the /Presto 0.172 Documentation/ .
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtswcTimeoutInMinutes :: Lens.Lens' DeltaTimeSessionWindowConfiguration Core.Natural
dtswcTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# INLINEABLE dtswcTimeoutInMinutes #-}
{-# DEPRECATED timeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead"  #-}

instance Core.FromJSON DeltaTimeSessionWindowConfiguration where
        toJSON DeltaTimeSessionWindowConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("timeoutInMinutes" Core..= timeoutInMinutes)])

instance Core.FromJSON DeltaTimeSessionWindowConfiguration where
        parseJSON
          = Core.withObject "DeltaTimeSessionWindowConfiguration" Core.$
              \ x ->
                DeltaTimeSessionWindowConfiguration' Core.<$>
                  (x Core..: "timeoutInMinutes")

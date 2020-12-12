{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
  ( DeltaTimeSessionWindowConfiguration (..),

    -- * Smart constructor
    mkDeltaTimeSessionWindowConfiguration,

    -- * Lenses
    dtswcTimeoutInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains the configuration information of a delta time session window.
--
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html @DeltaTime@ > specifies a time interval. You can use @DeltaTime@ to create dataset contents with data that has arrived in the data store since the last execution. For an example of @DeltaTime@ , see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/automate-create-dataset.html#automate-example6 Creating a SQL dataset with a delta window (CLI)> in the /AWS IoT Analytics User Guide/ .
--
-- /See:/ 'mkDeltaTimeSessionWindowConfiguration' smart constructor.
newtype DeltaTimeSessionWindowConfiguration = DeltaTimeSessionWindowConfiguration'
  { timeoutInMinutes ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeltaTimeSessionWindowConfiguration' with the minimum fields required to make a request.
--
-- * 'timeoutInMinutes' - A time interval. You can use @timeoutInMinutes@ so that AWS IoT Analytics can batch up late data notifications that have been generated since the last execution. AWS IoT Analytics sends one batch of notifications to Amazon CloudWatch Events at one time.
--
-- For more information about how to write a timestamp expression, see <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators> , in the /Presto 0.172 Documentation/ .
mkDeltaTimeSessionWindowConfiguration ::
  -- | 'timeoutInMinutes'
  Lude.Natural ->
  DeltaTimeSessionWindowConfiguration
mkDeltaTimeSessionWindowConfiguration pTimeoutInMinutes_ =
  DeltaTimeSessionWindowConfiguration'
    { timeoutInMinutes =
        pTimeoutInMinutes_
    }

-- | A time interval. You can use @timeoutInMinutes@ so that AWS IoT Analytics can batch up late data notifications that have been generated since the last execution. AWS IoT Analytics sends one batch of notifications to Amazon CloudWatch Events at one time.
--
-- For more information about how to write a timestamp expression, see <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators> , in the /Presto 0.172 Documentation/ .
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtswcTimeoutInMinutes :: Lens.Lens' DeltaTimeSessionWindowConfiguration Lude.Natural
dtswcTimeoutInMinutes = Lens.lens (timeoutInMinutes :: DeltaTimeSessionWindowConfiguration -> Lude.Natural) (\s a -> s {timeoutInMinutes = a} :: DeltaTimeSessionWindowConfiguration)
{-# DEPRECATED dtswcTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

instance Lude.FromJSON DeltaTimeSessionWindowConfiguration where
  parseJSON =
    Lude.withObject
      "DeltaTimeSessionWindowConfiguration"
      ( \x ->
          DeltaTimeSessionWindowConfiguration'
            Lude.<$> (x Lude..: "timeoutInMinutes")
      )

instance Lude.ToJSON DeltaTimeSessionWindowConfiguration where
  toJSON DeltaTimeSessionWindowConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("timeoutInMinutes" Lude..= timeoutInMinutes)]
      )

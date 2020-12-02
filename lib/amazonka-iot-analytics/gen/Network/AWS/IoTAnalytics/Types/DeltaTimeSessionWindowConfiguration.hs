{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains the configuration information of a delta time session window.
--
--
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html @DeltaTime@ > specifies a time interval. You can use @DeltaTime@ to create dataset contents with data that has arrived in the data store since the last execution. For an example of @DeltaTime@ , see <https://docs.aws.amazon.com/iotanalytics/latest/userguide/automate-create-dataset.html#automate-example6 Creating a SQL dataset with a delta window (CLI)> in the /AWS IoT Analytics User Guide/ .
--
--
-- /See:/ 'deltaTimeSessionWindowConfiguration' smart constructor.
newtype DeltaTimeSessionWindowConfiguration = DeltaTimeSessionWindowConfiguration'
  { _dtswcTimeoutInMinutes ::
      Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeltaTimeSessionWindowConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtswcTimeoutInMinutes' - A time interval. You can use @timeoutInMinutes@ so that AWS IoT Analytics can batch up late data notifications that have been generated since the last execution. AWS IoT Analytics sends one batch of notifications to Amazon CloudWatch Events at one time. For more information about how to write a timestamp expression, see <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators> , in the /Presto 0.172 Documentation/ .
deltaTimeSessionWindowConfiguration ::
  -- | 'dtswcTimeoutInMinutes'
  Natural ->
  DeltaTimeSessionWindowConfiguration
deltaTimeSessionWindowConfiguration pTimeoutInMinutes_ =
  DeltaTimeSessionWindowConfiguration'
    { _dtswcTimeoutInMinutes =
        _Nat # pTimeoutInMinutes_
    }

-- | A time interval. You can use @timeoutInMinutes@ so that AWS IoT Analytics can batch up late data notifications that have been generated since the last execution. AWS IoT Analytics sends one batch of notifications to Amazon CloudWatch Events at one time. For more information about how to write a timestamp expression, see <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators> , in the /Presto 0.172 Documentation/ .
dtswcTimeoutInMinutes :: Lens' DeltaTimeSessionWindowConfiguration Natural
dtswcTimeoutInMinutes = lens _dtswcTimeoutInMinutes (\s a -> s {_dtswcTimeoutInMinutes = a}) . _Nat

instance FromJSON DeltaTimeSessionWindowConfiguration where
  parseJSON =
    withObject
      "DeltaTimeSessionWindowConfiguration"
      ( \x ->
          DeltaTimeSessionWindowConfiguration' <$> (x .: "timeoutInMinutes")
      )

instance Hashable DeltaTimeSessionWindowConfiguration

instance NFData DeltaTimeSessionWindowConfiguration

instance ToJSON DeltaTimeSessionWindowConfiguration where
  toJSON DeltaTimeSessionWindowConfiguration' {..} =
    object
      (catMaybes [Just ("timeoutInMinutes" .= _dtswcTimeoutInMinutes)])

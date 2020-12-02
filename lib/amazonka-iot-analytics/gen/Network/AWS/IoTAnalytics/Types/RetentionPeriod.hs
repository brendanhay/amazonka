{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.RetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.RetentionPeriod where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | How long, in days, message data is kept.
--
--
--
-- /See:/ 'retentionPeriod' smart constructor.
data RetentionPeriod = RetentionPeriod'
  { _rpUnlimited ::
      !(Maybe Bool),
    _rpNumberOfDays :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RetentionPeriod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpUnlimited' - If true, message data is kept indefinitely.
--
-- * 'rpNumberOfDays' - The number of days that message data is kept. The @unlimited@ parameter must be false.
retentionPeriod ::
  RetentionPeriod
retentionPeriod =
  RetentionPeriod'
    { _rpUnlimited = Nothing,
      _rpNumberOfDays = Nothing
    }

-- | If true, message data is kept indefinitely.
rpUnlimited :: Lens' RetentionPeriod (Maybe Bool)
rpUnlimited = lens _rpUnlimited (\s a -> s {_rpUnlimited = a})

-- | The number of days that message data is kept. The @unlimited@ parameter must be false.
rpNumberOfDays :: Lens' RetentionPeriod (Maybe Natural)
rpNumberOfDays = lens _rpNumberOfDays (\s a -> s {_rpNumberOfDays = a}) . mapping _Nat

instance FromJSON RetentionPeriod where
  parseJSON =
    withObject
      "RetentionPeriod"
      ( \x ->
          RetentionPeriod'
            <$> (x .:? "unlimited") <*> (x .:? "numberOfDays")
      )

instance Hashable RetentionPeriod

instance NFData RetentionPeriod

instance ToJSON RetentionPeriod where
  toJSON RetentionPeriod' {..} =
    object
      ( catMaybes
          [ ("unlimited" .=) <$> _rpUnlimited,
            ("numberOfDays" .=) <$> _rpNumberOfDays
          ]
      )

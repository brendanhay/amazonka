{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.QuotaSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.QuotaSettings where

import Network.AWS.APIGateway.Types.QuotaPeriodType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Quotas configured for a usage plan.
--
--
--
-- /See:/ 'quotaSettings' smart constructor.
data QuotaSettings = QuotaSettings'
  { _qsOffset :: !(Maybe Int),
    _qsPeriod :: !(Maybe QuotaPeriodType),
    _qsLimit :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QuotaSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsOffset' - The number of requests subtracted from the given limit in the initial time period.
--
-- * 'qsPeriod' - The time period in which the limit applies. Valid values are "DAY", "WEEK" or "MONTH".
--
-- * 'qsLimit' - The maximum number of requests that can be made in a given time period.
quotaSettings ::
  QuotaSettings
quotaSettings =
  QuotaSettings'
    { _qsOffset = Nothing,
      _qsPeriod = Nothing,
      _qsLimit = Nothing
    }

-- | The number of requests subtracted from the given limit in the initial time period.
qsOffset :: Lens' QuotaSettings (Maybe Int)
qsOffset = lens _qsOffset (\s a -> s {_qsOffset = a})

-- | The time period in which the limit applies. Valid values are "DAY", "WEEK" or "MONTH".
qsPeriod :: Lens' QuotaSettings (Maybe QuotaPeriodType)
qsPeriod = lens _qsPeriod (\s a -> s {_qsPeriod = a})

-- | The maximum number of requests that can be made in a given time period.
qsLimit :: Lens' QuotaSettings (Maybe Int)
qsLimit = lens _qsLimit (\s a -> s {_qsLimit = a})

instance FromJSON QuotaSettings where
  parseJSON =
    withObject
      "QuotaSettings"
      ( \x ->
          QuotaSettings'
            <$> (x .:? "offset") <*> (x .:? "period") <*> (x .:? "limit")
      )

instance Hashable QuotaSettings

instance NFData QuotaSettings

instance ToJSON QuotaSettings where
  toJSON QuotaSettings' {..} =
    object
      ( catMaybes
          [ ("offset" .=) <$> _qsOffset,
            ("period" .=) <$> _qsPeriod,
            ("limit" .=) <$> _qsLimit
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about requests that failed with a 4xx Client Error status code.
--
--
--
-- /See:/ 'errorStatistics' smart constructor.
data ErrorStatistics = ErrorStatistics'
  { _eOtherCount ::
      !(Maybe Integer),
    _eThrottleCount :: !(Maybe Integer),
    _eTotalCount :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eOtherCount' - The number of requests that failed with untracked 4xx Client Error status codes.
--
-- * 'eThrottleCount' - The number of requests that failed with a 419 throttling status code.
--
-- * 'eTotalCount' - The total number of requests that failed with a 4xx Client Error status code.
errorStatistics ::
  ErrorStatistics
errorStatistics =
  ErrorStatistics'
    { _eOtherCount = Nothing,
      _eThrottleCount = Nothing,
      _eTotalCount = Nothing
    }

-- | The number of requests that failed with untracked 4xx Client Error status codes.
eOtherCount :: Lens' ErrorStatistics (Maybe Integer)
eOtherCount = lens _eOtherCount (\s a -> s {_eOtherCount = a})

-- | The number of requests that failed with a 419 throttling status code.
eThrottleCount :: Lens' ErrorStatistics (Maybe Integer)
eThrottleCount = lens _eThrottleCount (\s a -> s {_eThrottleCount = a})

-- | The total number of requests that failed with a 4xx Client Error status code.
eTotalCount :: Lens' ErrorStatistics (Maybe Integer)
eTotalCount = lens _eTotalCount (\s a -> s {_eTotalCount = a})

instance FromJSON ErrorStatistics where
  parseJSON =
    withObject
      "ErrorStatistics"
      ( \x ->
          ErrorStatistics'
            <$> (x .:? "OtherCount")
            <*> (x .:? "ThrottleCount")
            <*> (x .:? "TotalCount")
      )

instance Hashable ErrorStatistics

instance NFData ErrorStatistics

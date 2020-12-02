{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about requests that failed with a 5xx Server Error status code.
--
--
--
-- /See:/ 'faultStatistics' smart constructor.
data FaultStatistics = FaultStatistics'
  { _fsOtherCount ::
      !(Maybe Integer),
    _fsTotalCount :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FaultStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsOtherCount' - The number of requests that failed with untracked 5xx Server Error status codes.
--
-- * 'fsTotalCount' - The total number of requests that failed with a 5xx Server Error status code.
faultStatistics ::
  FaultStatistics
faultStatistics =
  FaultStatistics'
    { _fsOtherCount = Nothing,
      _fsTotalCount = Nothing
    }

-- | The number of requests that failed with untracked 5xx Server Error status codes.
fsOtherCount :: Lens' FaultStatistics (Maybe Integer)
fsOtherCount = lens _fsOtherCount (\s a -> s {_fsOtherCount = a})

-- | The total number of requests that failed with a 5xx Server Error status code.
fsTotalCount :: Lens' FaultStatistics (Maybe Integer)
fsTotalCount = lens _fsTotalCount (\s a -> s {_fsTotalCount = a})

instance FromJSON FaultStatistics where
  parseJSON =
    withObject
      "FaultStatistics"
      ( \x ->
          FaultStatistics' <$> (x .:? "OtherCount") <*> (x .:? "TotalCount")
      )

instance Hashable FaultStatistics

instance NFData FaultStatistics

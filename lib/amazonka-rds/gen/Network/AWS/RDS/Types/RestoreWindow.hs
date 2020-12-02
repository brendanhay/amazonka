{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.RestoreWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.RestoreWindow where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Earliest and latest time an instance can be restored to:
--
--
--
-- /See:/ 'restoreWindow' smart constructor.
data RestoreWindow = RestoreWindow'
  { _rwLatestTime ::
      !(Maybe ISO8601),
    _rwEarliestTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwLatestTime' - The latest time you can restore an instance to.
--
-- * 'rwEarliestTime' - The earliest time you can restore an instance to.
restoreWindow ::
  RestoreWindow
restoreWindow =
  RestoreWindow'
    { _rwLatestTime = Nothing,
      _rwEarliestTime = Nothing
    }

-- | The latest time you can restore an instance to.
rwLatestTime :: Lens' RestoreWindow (Maybe UTCTime)
rwLatestTime = lens _rwLatestTime (\s a -> s {_rwLatestTime = a}) . mapping _Time

-- | The earliest time you can restore an instance to.
rwEarliestTime :: Lens' RestoreWindow (Maybe UTCTime)
rwEarliestTime = lens _rwEarliestTime (\s a -> s {_rwEarliestTime = a}) . mapping _Time

instance FromXML RestoreWindow where
  parseXML x =
    RestoreWindow'
      <$> (x .@? "LatestTime") <*> (x .@? "EarliestTime")

instance Hashable RestoreWindow

instance NFData RestoreWindow

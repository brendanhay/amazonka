{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterBacktrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterBacktrack where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a response element in the @DescribeDBClusterBacktracks@ action.
--
--
--
-- /See:/ 'dbClusterBacktrack' smart constructor.
data DBClusterBacktrack = DBClusterBacktrack'
  { _dcbStatus ::
      !(Maybe Text),
    _dcbBacktrackIdentifier :: !(Maybe Text),
    _dcbBacktrackTo :: !(Maybe ISO8601),
    _dcbDBClusterIdentifier :: !(Maybe Text),
    _dcbBacktrackedFrom :: !(Maybe ISO8601),
    _dcbBacktrackRequestCreationTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterBacktrack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbStatus' - The status of the backtrack. This property returns one of the following values:     * @applying@ - The backtrack is currently being applied to or rolled back from the DB cluster.     * @completed@ - The backtrack has successfully been applied to or rolled back from the DB cluster.     * @failed@ - An error occurred while the backtrack was applied to or rolled back from the DB cluster.     * @pending@ - The backtrack is currently pending application to or rollback from the DB cluster.
--
-- * 'dcbBacktrackIdentifier' - Contains the backtrack identifier.
--
-- * 'dcbBacktrackTo' - The timestamp of the time to which the DB cluster was backtracked.
--
-- * 'dcbDBClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- * 'dcbBacktrackedFrom' - The timestamp of the time from which the DB cluster was backtracked.
--
-- * 'dcbBacktrackRequestCreationTime' - The timestamp of the time at which the backtrack was requested.
dbClusterBacktrack ::
  DBClusterBacktrack
dbClusterBacktrack =
  DBClusterBacktrack'
    { _dcbStatus = Nothing,
      _dcbBacktrackIdentifier = Nothing,
      _dcbBacktrackTo = Nothing,
      _dcbDBClusterIdentifier = Nothing,
      _dcbBacktrackedFrom = Nothing,
      _dcbBacktrackRequestCreationTime = Nothing
    }

-- | The status of the backtrack. This property returns one of the following values:     * @applying@ - The backtrack is currently being applied to or rolled back from the DB cluster.     * @completed@ - The backtrack has successfully been applied to or rolled back from the DB cluster.     * @failed@ - An error occurred while the backtrack was applied to or rolled back from the DB cluster.     * @pending@ - The backtrack is currently pending application to or rollback from the DB cluster.
dcbStatus :: Lens' DBClusterBacktrack (Maybe Text)
dcbStatus = lens _dcbStatus (\s a -> s {_dcbStatus = a})

-- | Contains the backtrack identifier.
dcbBacktrackIdentifier :: Lens' DBClusterBacktrack (Maybe Text)
dcbBacktrackIdentifier = lens _dcbBacktrackIdentifier (\s a -> s {_dcbBacktrackIdentifier = a})

-- | The timestamp of the time to which the DB cluster was backtracked.
dcbBacktrackTo :: Lens' DBClusterBacktrack (Maybe UTCTime)
dcbBacktrackTo = lens _dcbBacktrackTo (\s a -> s {_dcbBacktrackTo = a}) . mapping _Time

-- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
dcbDBClusterIdentifier :: Lens' DBClusterBacktrack (Maybe Text)
dcbDBClusterIdentifier = lens _dcbDBClusterIdentifier (\s a -> s {_dcbDBClusterIdentifier = a})

-- | The timestamp of the time from which the DB cluster was backtracked.
dcbBacktrackedFrom :: Lens' DBClusterBacktrack (Maybe UTCTime)
dcbBacktrackedFrom = lens _dcbBacktrackedFrom (\s a -> s {_dcbBacktrackedFrom = a}) . mapping _Time

-- | The timestamp of the time at which the backtrack was requested.
dcbBacktrackRequestCreationTime :: Lens' DBClusterBacktrack (Maybe UTCTime)
dcbBacktrackRequestCreationTime = lens _dcbBacktrackRequestCreationTime (\s a -> s {_dcbBacktrackRequestCreationTime = a}) . mapping _Time

instance FromXML DBClusterBacktrack where
  parseXML x =
    DBClusterBacktrack'
      <$> (x .@? "Status")
      <*> (x .@? "BacktrackIdentifier")
      <*> (x .@? "BacktrackTo")
      <*> (x .@? "DBClusterIdentifier")
      <*> (x .@? "BacktrackedFrom")
      <*> (x .@? "BacktrackRequestCreationTime")

instance Hashable DBClusterBacktrack

instance NFData DBClusterBacktrack

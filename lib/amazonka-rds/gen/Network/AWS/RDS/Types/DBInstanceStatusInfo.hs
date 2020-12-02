{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBInstanceStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstanceStatusInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a list of status information for a DB instance.
--
--
--
-- /See:/ 'dbInstanceStatusInfo' smart constructor.
data DBInstanceStatusInfo = DBInstanceStatusInfo'
  { _disiStatus ::
      !(Maybe Text),
    _disiNormal :: !(Maybe Bool),
    _disiStatusType :: !(Maybe Text),
    _disiMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBInstanceStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disiStatus' - Status of the DB instance. For a StatusType of read replica, the values can be replicating, replication stop point set, replication stop point reached, error, stopped, or terminated.
--
-- * 'disiNormal' - Boolean value that is true if the instance is operating normally, or false if the instance is in an error state.
--
-- * 'disiStatusType' - This value is currently "read replication."
--
-- * 'disiMessage' - Details of the error if there is an error for the instance. If the instance isn't in an error state, this value is blank.
dbInstanceStatusInfo ::
  DBInstanceStatusInfo
dbInstanceStatusInfo =
  DBInstanceStatusInfo'
    { _disiStatus = Nothing,
      _disiNormal = Nothing,
      _disiStatusType = Nothing,
      _disiMessage = Nothing
    }

-- | Status of the DB instance. For a StatusType of read replica, the values can be replicating, replication stop point set, replication stop point reached, error, stopped, or terminated.
disiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatus = lens _disiStatus (\s a -> s {_disiStatus = a})

-- | Boolean value that is true if the instance is operating normally, or false if the instance is in an error state.
disiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
disiNormal = lens _disiNormal (\s a -> s {_disiNormal = a})

-- | This value is currently "read replication."
disiStatusType :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatusType = lens _disiStatusType (\s a -> s {_disiStatusType = a})

-- | Details of the error if there is an error for the instance. If the instance isn't in an error state, this value is blank.
disiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
disiMessage = lens _disiMessage (\s a -> s {_disiMessage = a})

instance FromXML DBInstanceStatusInfo where
  parseXML x =
    DBInstanceStatusInfo'
      <$> (x .@? "Status")
      <*> (x .@? "Normal")
      <*> (x .@? "StatusType")
      <*> (x .@? "Message")

instance Hashable DBInstanceStatusInfo

instance NFData DBInstanceStatusInfo

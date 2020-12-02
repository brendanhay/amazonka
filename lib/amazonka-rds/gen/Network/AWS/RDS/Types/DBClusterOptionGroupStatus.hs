{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterOptionGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterOptionGroupStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains status information for a DB cluster option group.
--
--
--
-- /See:/ 'dbClusterOptionGroupStatus' smart constructor.
data DBClusterOptionGroupStatus = DBClusterOptionGroupStatus'
  { _dcogsStatus ::
      !(Maybe Text),
    _dcogsDBClusterOptionGroupName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterOptionGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcogsStatus' - Specifies the status of the DB cluster option group.
--
-- * 'dcogsDBClusterOptionGroupName' - Specifies the name of the DB cluster option group.
dbClusterOptionGroupStatus ::
  DBClusterOptionGroupStatus
dbClusterOptionGroupStatus =
  DBClusterOptionGroupStatus'
    { _dcogsStatus = Nothing,
      _dcogsDBClusterOptionGroupName = Nothing
    }

-- | Specifies the status of the DB cluster option group.
dcogsStatus :: Lens' DBClusterOptionGroupStatus (Maybe Text)
dcogsStatus = lens _dcogsStatus (\s a -> s {_dcogsStatus = a})

-- | Specifies the name of the DB cluster option group.
dcogsDBClusterOptionGroupName :: Lens' DBClusterOptionGroupStatus (Maybe Text)
dcogsDBClusterOptionGroupName = lens _dcogsDBClusterOptionGroupName (\s a -> s {_dcogsDBClusterOptionGroupName = a})

instance FromXML DBClusterOptionGroupStatus where
  parseXML x =
    DBClusterOptionGroupStatus'
      <$> (x .@? "Status") <*> (x .@? "DBClusterOptionGroupName")

instance Hashable DBClusterOptionGroupStatus

instance NFData DBClusterOptionGroupStatus

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBParameterGroupStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of the DB parameter group.
--
--
-- This data type is used as a response element in the following actions:
--
--     * @CreateDBInstance@
--
--     * @CreateDBInstanceReadReplica@
--
--     * @DeleteDBInstance@
--
--     * @ModifyDBInstance@
--
--     * @RebootDBInstance@
--
--     * @RestoreDBInstanceFromDBSnapshot@
--
--
--
--
-- /See:/ 'dbParameterGroupStatus' smart constructor.
data DBParameterGroupStatus = DBParameterGroupStatus'
  { _dpgsDBParameterGroupName ::
      !(Maybe Text),
    _dpgsParameterApplyStatus :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBParameterGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgsDBParameterGroupName' - The name of the DB parameter group.
--
-- * 'dpgsParameterApplyStatus' - The status of parameter updates.
dbParameterGroupStatus ::
  DBParameterGroupStatus
dbParameterGroupStatus =
  DBParameterGroupStatus'
    { _dpgsDBParameterGroupName = Nothing,
      _dpgsParameterApplyStatus = Nothing
    }

-- | The name of the DB parameter group.
dpgsDBParameterGroupName :: Lens' DBParameterGroupStatus (Maybe Text)
dpgsDBParameterGroupName = lens _dpgsDBParameterGroupName (\s a -> s {_dpgsDBParameterGroupName = a})

-- | The status of parameter updates.
dpgsParameterApplyStatus :: Lens' DBParameterGroupStatus (Maybe Text)
dpgsParameterApplyStatus = lens _dpgsParameterApplyStatus (\s a -> s {_dpgsParameterApplyStatus = a})

instance FromXML DBParameterGroupStatus where
  parseXML x =
    DBParameterGroupStatus'
      <$> (x .@? "DBParameterGroupName") <*> (x .@? "ParameterApplyStatus")

instance Hashable DBParameterGroupStatus

instance NFData DBParameterGroupStatus

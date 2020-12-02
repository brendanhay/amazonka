{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'dbClusterParameterGroupNameMessage' smart constructor.
newtype DBClusterParameterGroupNameMessage = DBClusterParameterGroupNameMessage'
  { _dcpgnmDBClusterParameterGroupName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgnmDBClusterParameterGroupName' - The name of the DB cluster parameter group. Constraints:     * Must be 1 to 255 letters or numbers.     * First character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens
dbClusterParameterGroupNameMessage ::
  DBClusterParameterGroupNameMessage
dbClusterParameterGroupNameMessage =
  DBClusterParameterGroupNameMessage'
    { _dcpgnmDBClusterParameterGroupName =
        Nothing
    }

-- | The name of the DB cluster parameter group. Constraints:     * Must be 1 to 255 letters or numbers.     * First character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens
dcpgnmDBClusterParameterGroupName :: Lens' DBClusterParameterGroupNameMessage (Maybe Text)
dcpgnmDBClusterParameterGroupName = lens _dcpgnmDBClusterParameterGroupName (\s a -> s {_dcpgnmDBClusterParameterGroupName = a})

instance FromXML DBClusterParameterGroupNameMessage where
  parseXML x =
    DBClusterParameterGroupNameMessage'
      <$> (x .@? "DBClusterParameterGroupName")

instance Hashable DBClusterParameterGroupNameMessage

instance NFData DBClusterParameterGroupNameMessage

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBParameterGroupNameMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the result of a successful invocation of the @ModifyDBParameterGroup@ or @ResetDBParameterGroup@ action.
--
--
--
-- /See:/ 'dbParameterGroupNameMessage' smart constructor.
newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage'
  { _dpgnmDBParameterGroupName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgnmDBParameterGroupName' - The name of the DB parameter group.
dbParameterGroupNameMessage ::
  DBParameterGroupNameMessage
dbParameterGroupNameMessage =
  DBParameterGroupNameMessage'
    { _dpgnmDBParameterGroupName =
        Nothing
    }

-- | The name of the DB parameter group.
dpgnmDBParameterGroupName :: Lens' DBParameterGroupNameMessage (Maybe Text)
dpgnmDBParameterGroupName = lens _dpgnmDBParameterGroupName (\s a -> s {_dpgnmDBParameterGroupName = a})

instance FromXML DBParameterGroupNameMessage where
  parseXML x =
    DBParameterGroupNameMessage' <$> (x .@? "DBParameterGroupName")

instance Hashable DBParameterGroupNameMessage

instance NFData DBParameterGroupNameMessage

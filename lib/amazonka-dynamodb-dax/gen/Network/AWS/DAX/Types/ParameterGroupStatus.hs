{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ParameterGroupStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of a parameter group.
--
--
--
-- /See:/ 'parameterGroupStatus' smart constructor.
data ParameterGroupStatus = ParameterGroupStatus'
  { _pgsNodeIdsToReboot ::
      !(Maybe [Text]),
    _pgsParameterApplyStatus :: !(Maybe Text),
    _pgsParameterGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgsNodeIdsToReboot' - The node IDs of one or more nodes to be rebooted.
--
-- * 'pgsParameterApplyStatus' - The status of parameter updates.
--
-- * 'pgsParameterGroupName' - The name of the parameter group.
parameterGroupStatus ::
  ParameterGroupStatus
parameterGroupStatus =
  ParameterGroupStatus'
    { _pgsNodeIdsToReboot = Nothing,
      _pgsParameterApplyStatus = Nothing,
      _pgsParameterGroupName = Nothing
    }

-- | The node IDs of one or more nodes to be rebooted.
pgsNodeIdsToReboot :: Lens' ParameterGroupStatus [Text]
pgsNodeIdsToReboot = lens _pgsNodeIdsToReboot (\s a -> s {_pgsNodeIdsToReboot = a}) . _Default . _Coerce

-- | The status of parameter updates.
pgsParameterApplyStatus :: Lens' ParameterGroupStatus (Maybe Text)
pgsParameterApplyStatus = lens _pgsParameterApplyStatus (\s a -> s {_pgsParameterApplyStatus = a})

-- | The name of the parameter group.
pgsParameterGroupName :: Lens' ParameterGroupStatus (Maybe Text)
pgsParameterGroupName = lens _pgsParameterGroupName (\s a -> s {_pgsParameterGroupName = a})

instance FromJSON ParameterGroupStatus where
  parseJSON =
    withObject
      "ParameterGroupStatus"
      ( \x ->
          ParameterGroupStatus'
            <$> (x .:? "NodeIdsToReboot" .!= mempty)
            <*> (x .:? "ParameterApplyStatus")
            <*> (x .:? "ParameterGroupName")
      )

instance Hashable ParameterGroupStatus

instance NFData ParameterGroupStatus

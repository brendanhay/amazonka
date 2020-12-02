{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroup where

import Network.AWS.Athena.Types.WorkGroupConfiguration
import Network.AWS.Athena.Types.WorkGroupState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A workgroup, which contains a name, description, creation time, state, and other configuration, listed under 'WorkGroup$Configuration' . Each workgroup enables you to isolate queries for you or your group of users from other queries in the same account, to configure the query results location and the encryption configuration (known as workgroup settings), to enable sending query metrics to Amazon CloudWatch, and to establish per-query data usage control limits for all queries in a workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
--
--
-- /See:/ 'workGroup' smart constructor.
data WorkGroup = WorkGroup'
  { _wgCreationTime :: !(Maybe POSIX),
    _wgState :: !(Maybe WorkGroupState),
    _wgConfiguration :: !(Maybe WorkGroupConfiguration),
    _wgDescription :: !(Maybe Text),
    _wgName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wgCreationTime' - The date and time the workgroup was created.
--
-- * 'wgState' - The state of the workgroup: ENABLED or DISABLED.
--
-- * 'wgConfiguration' - The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- * 'wgDescription' - The workgroup description.
--
-- * 'wgName' - The workgroup name.
workGroup ::
  -- | 'wgName'
  Text ->
  WorkGroup
workGroup pName_ =
  WorkGroup'
    { _wgCreationTime = Nothing,
      _wgState = Nothing,
      _wgConfiguration = Nothing,
      _wgDescription = Nothing,
      _wgName = pName_
    }

-- | The date and time the workgroup was created.
wgCreationTime :: Lens' WorkGroup (Maybe UTCTime)
wgCreationTime = lens _wgCreationTime (\s a -> s {_wgCreationTime = a}) . mapping _Time

-- | The state of the workgroup: ENABLED or DISABLED.
wgState :: Lens' WorkGroup (Maybe WorkGroupState)
wgState = lens _wgState (\s a -> s {_wgState = a})

-- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
wgConfiguration :: Lens' WorkGroup (Maybe WorkGroupConfiguration)
wgConfiguration = lens _wgConfiguration (\s a -> s {_wgConfiguration = a})

-- | The workgroup description.
wgDescription :: Lens' WorkGroup (Maybe Text)
wgDescription = lens _wgDescription (\s a -> s {_wgDescription = a})

-- | The workgroup name.
wgName :: Lens' WorkGroup Text
wgName = lens _wgName (\s a -> s {_wgName = a})

instance FromJSON WorkGroup where
  parseJSON =
    withObject
      "WorkGroup"
      ( \x ->
          WorkGroup'
            <$> (x .:? "CreationTime")
            <*> (x .:? "State")
            <*> (x .:? "Configuration")
            <*> (x .:? "Description")
            <*> (x .: "Name")
      )

instance Hashable WorkGroup

instance NFData WorkGroup

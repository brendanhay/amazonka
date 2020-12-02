{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupSummary where

import Network.AWS.Athena.Types.WorkGroupState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary information for the workgroup, which includes its name, state, description, and the date and time it was created.
--
--
--
-- /See:/ 'workGroupSummary' smart constructor.
data WorkGroupSummary = WorkGroupSummary'
  { _wgsCreationTime ::
      !(Maybe POSIX),
    _wgsState :: !(Maybe WorkGroupState),
    _wgsName :: !(Maybe Text),
    _wgsDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wgsCreationTime' - The workgroup creation date and time.
--
-- * 'wgsState' - The state of the workgroup.
--
-- * 'wgsName' - The name of the workgroup.
--
-- * 'wgsDescription' - The workgroup description.
workGroupSummary ::
  WorkGroupSummary
workGroupSummary =
  WorkGroupSummary'
    { _wgsCreationTime = Nothing,
      _wgsState = Nothing,
      _wgsName = Nothing,
      _wgsDescription = Nothing
    }

-- | The workgroup creation date and time.
wgsCreationTime :: Lens' WorkGroupSummary (Maybe UTCTime)
wgsCreationTime = lens _wgsCreationTime (\s a -> s {_wgsCreationTime = a}) . mapping _Time

-- | The state of the workgroup.
wgsState :: Lens' WorkGroupSummary (Maybe WorkGroupState)
wgsState = lens _wgsState (\s a -> s {_wgsState = a})

-- | The name of the workgroup.
wgsName :: Lens' WorkGroupSummary (Maybe Text)
wgsName = lens _wgsName (\s a -> s {_wgsName = a})

-- | The workgroup description.
wgsDescription :: Lens' WorkGroupSummary (Maybe Text)
wgsDescription = lens _wgsDescription (\s a -> s {_wgsDescription = a})

instance FromJSON WorkGroupSummary where
  parseJSON =
    withObject
      "WorkGroupSummary"
      ( \x ->
          WorkGroupSummary'
            <$> (x .:? "CreationTime")
            <*> (x .:? "State")
            <*> (x .:? "Name")
            <*> (x .:? "Description")
      )

instance Hashable WorkGroupSummary

instance NFData WorkGroupSummary

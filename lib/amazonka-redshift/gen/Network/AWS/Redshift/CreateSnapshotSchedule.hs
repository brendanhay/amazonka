{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a snapshot schedule that can be associated to a cluster and which overrides the default system backup schedule.
module Network.AWS.Redshift.CreateSnapshotSchedule
  ( -- * Creating a Request
    createSnapshotSchedule,
    CreateSnapshotSchedule,

    -- * Request Lenses
    cssNextInvocations,
    cssScheduleDefinitions,
    cssScheduleDescription,
    cssScheduleIdentifier,
    cssDryRun,
    cssTags,

    -- * Destructuring the Response
    snapshotSchedule,
    SnapshotSchedule,

    -- * Response Lenses
    ssAssociatedClusters,
    ssNextInvocations,
    ssScheduleDefinitions,
    ssScheduleDescription,
    ssScheduleIdentifier,
    ssAssociatedClusterCount,
    ssTags,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSnapshotSchedule' smart constructor.
data CreateSnapshotSchedule = CreateSnapshotSchedule'
  { _cssNextInvocations ::
      !(Maybe Int),
    _cssScheduleDefinitions :: !(Maybe [Text]),
    _cssScheduleDescription :: !(Maybe Text),
    _cssScheduleIdentifier :: !(Maybe Text),
    _cssDryRun :: !(Maybe Bool),
    _cssTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssNextInvocations' -
--
-- * 'cssScheduleDefinitions' - The definition of the snapshot schedule. The definition is made up of schedule expressions, for example "cron(30 12 *)" or "rate(12 hours)".
--
-- * 'cssScheduleDescription' - The description of the snapshot schedule.
--
-- * 'cssScheduleIdentifier' - A unique identifier for a snapshot schedule. Only alphanumeric characters are allowed for the identifier.
--
-- * 'cssDryRun' -
--
-- * 'cssTags' - An optional set of tags you can use to search for the schedule.
createSnapshotSchedule ::
  CreateSnapshotSchedule
createSnapshotSchedule =
  CreateSnapshotSchedule'
    { _cssNextInvocations = Nothing,
      _cssScheduleDefinitions = Nothing,
      _cssScheduleDescription = Nothing,
      _cssScheduleIdentifier = Nothing,
      _cssDryRun = Nothing,
      _cssTags = Nothing
    }

-- |
cssNextInvocations :: Lens' CreateSnapshotSchedule (Maybe Int)
cssNextInvocations = lens _cssNextInvocations (\s a -> s {_cssNextInvocations = a})

-- | The definition of the snapshot schedule. The definition is made up of schedule expressions, for example "cron(30 12 *)" or "rate(12 hours)".
cssScheduleDefinitions :: Lens' CreateSnapshotSchedule [Text]
cssScheduleDefinitions = lens _cssScheduleDefinitions (\s a -> s {_cssScheduleDefinitions = a}) . _Default . _Coerce

-- | The description of the snapshot schedule.
cssScheduleDescription :: Lens' CreateSnapshotSchedule (Maybe Text)
cssScheduleDescription = lens _cssScheduleDescription (\s a -> s {_cssScheduleDescription = a})

-- | A unique identifier for a snapshot schedule. Only alphanumeric characters are allowed for the identifier.
cssScheduleIdentifier :: Lens' CreateSnapshotSchedule (Maybe Text)
cssScheduleIdentifier = lens _cssScheduleIdentifier (\s a -> s {_cssScheduleIdentifier = a})

-- |
cssDryRun :: Lens' CreateSnapshotSchedule (Maybe Bool)
cssDryRun = lens _cssDryRun (\s a -> s {_cssDryRun = a})

-- | An optional set of tags you can use to search for the schedule.
cssTags :: Lens' CreateSnapshotSchedule [Tag]
cssTags = lens _cssTags (\s a -> s {_cssTags = a}) . _Default . _Coerce

instance AWSRequest CreateSnapshotSchedule where
  type Rs CreateSnapshotSchedule = SnapshotSchedule
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "CreateSnapshotScheduleResult"
      (\s h x -> parseXML x)

instance Hashable CreateSnapshotSchedule

instance NFData CreateSnapshotSchedule

instance ToHeaders CreateSnapshotSchedule where
  toHeaders = const mempty

instance ToPath CreateSnapshotSchedule where
  toPath = const "/"

instance ToQuery CreateSnapshotSchedule where
  toQuery CreateSnapshotSchedule' {..} =
    mconcat
      [ "Action" =: ("CreateSnapshotSchedule" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "NextInvocations" =: _cssNextInvocations,
        "ScheduleDefinitions"
          =: toQuery
            (toQueryList "ScheduleDefinition" <$> _cssScheduleDefinitions),
        "ScheduleDescription" =: _cssScheduleDescription,
        "ScheduleIdentifier" =: _cssScheduleIdentifier,
        "DryRun" =: _cssDryRun,
        "Tags" =: toQuery (toQueryList "Tag" <$> _cssTags)
      ]

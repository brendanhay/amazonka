{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifySnapshotSchedule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a snapshot schedule. Any schedule associated with a cluster is modified asynchronously.
--
--
module Network.AWS.Redshift.ModifySnapshotSchedule
    (
    -- * Creating a Request
      modifySnapshotSchedule
    , ModifySnapshotSchedule
    -- * Request Lenses
    , mssScheduleIdentifier
    , mssScheduleDefinitions

    -- * Destructuring the Response
    , snapshotSchedule
    , SnapshotSchedule
    -- * Response Lenses
    , ssAssociatedClusters
    , ssNextInvocations
    , ssScheduleDefinitions
    , ssScheduleDescription
    , ssScheduleIdentifier
    , ssAssociatedClusterCount
    , ssTags
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifySnapshotSchedule' smart constructor.
data ModifySnapshotSchedule = ModifySnapshotSchedule'
  { _mssScheduleIdentifier  :: !Text
  , _mssScheduleDefinitions :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifySnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssScheduleIdentifier' - A unique alphanumeric identifier of the schedule to modify.
--
-- * 'mssScheduleDefinitions' - An updated list of schedule definitions. A schedule definition is made up of schedule expressions, for example, "cron(30 12 *)" or "rate(12 hours)".
modifySnapshotSchedule
    :: Text -- ^ 'mssScheduleIdentifier'
    -> ModifySnapshotSchedule
modifySnapshotSchedule pScheduleIdentifier_ =
  ModifySnapshotSchedule'
    { _mssScheduleIdentifier = pScheduleIdentifier_
    , _mssScheduleDefinitions = mempty
    }


-- | A unique alphanumeric identifier of the schedule to modify.
mssScheduleIdentifier :: Lens' ModifySnapshotSchedule Text
mssScheduleIdentifier = lens _mssScheduleIdentifier (\ s a -> s{_mssScheduleIdentifier = a})

-- | An updated list of schedule definitions. A schedule definition is made up of schedule expressions, for example, "cron(30 12 *)" or "rate(12 hours)".
mssScheduleDefinitions :: Lens' ModifySnapshotSchedule [Text]
mssScheduleDefinitions = lens _mssScheduleDefinitions (\ s a -> s{_mssScheduleDefinitions = a}) . _Coerce

instance AWSRequest ModifySnapshotSchedule where
        type Rs ModifySnapshotSchedule = SnapshotSchedule
        request = postQuery redshift
        response
          = receiveXMLWrapper "ModifySnapshotScheduleResult"
              (\ s h x -> parseXML x)

instance Hashable ModifySnapshotSchedule where

instance NFData ModifySnapshotSchedule where

instance ToHeaders ModifySnapshotSchedule where
        toHeaders = const mempty

instance ToPath ModifySnapshotSchedule where
        toPath = const "/"

instance ToQuery ModifySnapshotSchedule where
        toQuery ModifySnapshotSchedule'{..}
          = mconcat
              ["Action" =:
                 ("ModifySnapshotSchedule" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ScheduleIdentifier" =: _mssScheduleIdentifier,
               "ScheduleDefinitions" =:
                 toQueryList "ScheduleDefinition"
                   _mssScheduleDefinitions]

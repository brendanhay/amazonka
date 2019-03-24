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
-- Module      : Network.AWS.Redshift.ModifyClusterSnapshotSchedule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a snapshot schedule for a cluster.
--
--
module Network.AWS.Redshift.ModifyClusterSnapshotSchedule
    (
    -- * Creating a Request
      modifyClusterSnapshotSchedule
    , ModifyClusterSnapshotSchedule
    -- * Request Lenses
    , mcssDisassociateSchedule
    , mcssScheduleIdentifier
    , mcssClusterIdentifier

    -- * Destructuring the Response
    , modifyClusterSnapshotScheduleResponse
    , ModifyClusterSnapshotScheduleResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyClusterSnapshotSchedule' smart constructor.
data ModifyClusterSnapshotSchedule = ModifyClusterSnapshotSchedule'
  { _mcssDisassociateSchedule :: !(Maybe Bool)
  , _mcssScheduleIdentifier   :: !(Maybe Text)
  , _mcssClusterIdentifier    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClusterSnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcssDisassociateSchedule' - A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
--
-- * 'mcssScheduleIdentifier' - A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
--
-- * 'mcssClusterIdentifier' - A unique identifier for the cluster whose snapshot schedule you want to modify.
modifyClusterSnapshotSchedule
    :: Text -- ^ 'mcssClusterIdentifier'
    -> ModifyClusterSnapshotSchedule
modifyClusterSnapshotSchedule pClusterIdentifier_ =
  ModifyClusterSnapshotSchedule'
    { _mcssDisassociateSchedule = Nothing
    , _mcssScheduleIdentifier = Nothing
    , _mcssClusterIdentifier = pClusterIdentifier_
    }


-- | A boolean to indicate whether to remove the assoiciation between the cluster and the schedule.
mcssDisassociateSchedule :: Lens' ModifyClusterSnapshotSchedule (Maybe Bool)
mcssDisassociateSchedule = lens _mcssDisassociateSchedule (\ s a -> s{_mcssDisassociateSchedule = a})

-- | A unique alphanumeric identifier for the schedule that you want to associate with the cluster.
mcssScheduleIdentifier :: Lens' ModifyClusterSnapshotSchedule (Maybe Text)
mcssScheduleIdentifier = lens _mcssScheduleIdentifier (\ s a -> s{_mcssScheduleIdentifier = a})

-- | A unique identifier for the cluster whose snapshot schedule you want to modify.
mcssClusterIdentifier :: Lens' ModifyClusterSnapshotSchedule Text
mcssClusterIdentifier = lens _mcssClusterIdentifier (\ s a -> s{_mcssClusterIdentifier = a})

instance AWSRequest ModifyClusterSnapshotSchedule
         where
        type Rs ModifyClusterSnapshotSchedule =
             ModifyClusterSnapshotScheduleResponse
        request = postQuery redshift
        response
          = receiveNull ModifyClusterSnapshotScheduleResponse'

instance Hashable ModifyClusterSnapshotSchedule where

instance NFData ModifyClusterSnapshotSchedule where

instance ToHeaders ModifyClusterSnapshotSchedule
         where
        toHeaders = const mempty

instance ToPath ModifyClusterSnapshotSchedule where
        toPath = const "/"

instance ToQuery ModifyClusterSnapshotSchedule where
        toQuery ModifyClusterSnapshotSchedule'{..}
          = mconcat
              ["Action" =:
                 ("ModifyClusterSnapshotSchedule" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "DisassociateSchedule" =: _mcssDisassociateSchedule,
               "ScheduleIdentifier" =: _mcssScheduleIdentifier,
               "ClusterIdentifier" =: _mcssClusterIdentifier]

-- | /See:/ 'modifyClusterSnapshotScheduleResponse' smart constructor.
data ModifyClusterSnapshotScheduleResponse =
  ModifyClusterSnapshotScheduleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClusterSnapshotScheduleResponse' with the minimum fields required to make a request.
--
modifyClusterSnapshotScheduleResponse
    :: ModifyClusterSnapshotScheduleResponse
modifyClusterSnapshotScheduleResponse = ModifyClusterSnapshotScheduleResponse'


instance NFData ModifyClusterSnapshotScheduleResponse
         where

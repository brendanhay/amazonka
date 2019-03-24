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
-- Module      : Network.AWS.Redshift.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot schedule.
--
--
module Network.AWS.Redshift.DeleteSnapshotSchedule
    (
    -- * Creating a Request
      deleteSnapshotSchedule
    , DeleteSnapshotSchedule
    -- * Request Lenses
    , dScheduleIdentifier

    -- * Destructuring the Response
    , deleteSnapshotScheduleResponse
    , DeleteSnapshotScheduleResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSnapshotSchedule' smart constructor.
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { _dScheduleIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dScheduleIdentifier' - A unique identifier of the snapshot schedule to delete.
deleteSnapshotSchedule
    :: Text -- ^ 'dScheduleIdentifier'
    -> DeleteSnapshotSchedule
deleteSnapshotSchedule pScheduleIdentifier_ =
  DeleteSnapshotSchedule' {_dScheduleIdentifier = pScheduleIdentifier_}


-- | A unique identifier of the snapshot schedule to delete.
dScheduleIdentifier :: Lens' DeleteSnapshotSchedule Text
dScheduleIdentifier = lens _dScheduleIdentifier (\ s a -> s{_dScheduleIdentifier = a})

instance AWSRequest DeleteSnapshotSchedule where
        type Rs DeleteSnapshotSchedule =
             DeleteSnapshotScheduleResponse
        request = postQuery redshift
        response
          = receiveNull DeleteSnapshotScheduleResponse'

instance Hashable DeleteSnapshotSchedule where

instance NFData DeleteSnapshotSchedule where

instance ToHeaders DeleteSnapshotSchedule where
        toHeaders = const mempty

instance ToPath DeleteSnapshotSchedule where
        toPath = const "/"

instance ToQuery DeleteSnapshotSchedule where
        toQuery DeleteSnapshotSchedule'{..}
          = mconcat
              ["Action" =:
                 ("DeleteSnapshotSchedule" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ScheduleIdentifier" =: _dScheduleIdentifier]

-- | /See:/ 'deleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse =
  DeleteSnapshotScheduleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSnapshotScheduleResponse' with the minimum fields required to make a request.
--
deleteSnapshotScheduleResponse
    :: DeleteSnapshotScheduleResponse
deleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'


instance NFData DeleteSnapshotScheduleResponse where

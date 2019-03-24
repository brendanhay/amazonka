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
-- Module      : Network.AWS.Redshift.ModifyClusterSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a snapshot.
--
--
module Network.AWS.Redshift.ModifyClusterSnapshot
    (
    -- * Creating a Request
      modifyClusterSnapshot
    , ModifyClusterSnapshot
    -- * Request Lenses
    , mcsManualSnapshotRetentionPeriod
    , mcsForce
    , mcsSnapshotIdentifier

    -- * Destructuring the Response
    , modifyClusterSnapshotResponse
    , ModifyClusterSnapshotResponse
    -- * Response Lenses
    , mcsrsSnapshot
    , mcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyClusterSnapshot' smart constructor.
data ModifyClusterSnapshot = ModifyClusterSnapshot'
  { _mcsManualSnapshotRetentionPeriod :: !(Maybe Int)
  , _mcsForce                         :: !(Maybe Bool)
  , _mcsSnapshotIdentifier            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcsManualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. If the manual snapshot falls outside of the new retention period, you can specify the force option to immediately delete the snapshot. The value must be either -1 or an integer between 1 and 3,653.
--
-- * 'mcsForce' - A Boolean option to override an exception if the retention period has already passed.
--
-- * 'mcsSnapshotIdentifier' - The identifier of the snapshot whose setting you want to modify.
modifyClusterSnapshot
    :: Text -- ^ 'mcsSnapshotIdentifier'
    -> ModifyClusterSnapshot
modifyClusterSnapshot pSnapshotIdentifier_ =
  ModifyClusterSnapshot'
    { _mcsManualSnapshotRetentionPeriod = Nothing
    , _mcsForce = Nothing
    , _mcsSnapshotIdentifier = pSnapshotIdentifier_
    }


-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely. If the manual snapshot falls outside of the new retention period, you can specify the force option to immediately delete the snapshot. The value must be either -1 or an integer between 1 and 3,653.
mcsManualSnapshotRetentionPeriod :: Lens' ModifyClusterSnapshot (Maybe Int)
mcsManualSnapshotRetentionPeriod = lens _mcsManualSnapshotRetentionPeriod (\ s a -> s{_mcsManualSnapshotRetentionPeriod = a})

-- | A Boolean option to override an exception if the retention period has already passed.
mcsForce :: Lens' ModifyClusterSnapshot (Maybe Bool)
mcsForce = lens _mcsForce (\ s a -> s{_mcsForce = a})

-- | The identifier of the snapshot whose setting you want to modify.
mcsSnapshotIdentifier :: Lens' ModifyClusterSnapshot Text
mcsSnapshotIdentifier = lens _mcsSnapshotIdentifier (\ s a -> s{_mcsSnapshotIdentifier = a})

instance AWSRequest ModifyClusterSnapshot where
        type Rs ModifyClusterSnapshot =
             ModifyClusterSnapshotResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "ModifyClusterSnapshotResult"
              (\ s h x ->
                 ModifyClusterSnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance Hashable ModifyClusterSnapshot where

instance NFData ModifyClusterSnapshot where

instance ToHeaders ModifyClusterSnapshot where
        toHeaders = const mempty

instance ToPath ModifyClusterSnapshot where
        toPath = const "/"

instance ToQuery ModifyClusterSnapshot where
        toQuery ModifyClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("ModifyClusterSnapshot" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ManualSnapshotRetentionPeriod" =:
                 _mcsManualSnapshotRetentionPeriod,
               "Force" =: _mcsForce,
               "SnapshotIdentifier" =: _mcsSnapshotIdentifier]

-- | /See:/ 'modifyClusterSnapshotResponse' smart constructor.
data ModifyClusterSnapshotResponse = ModifyClusterSnapshotResponse'
  { _mcsrsSnapshot       :: !(Maybe Snapshot)
  , _mcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcsrsSnapshot' - Undocumented member.
--
-- * 'mcsrsResponseStatus' - -- | The response status code.
modifyClusterSnapshotResponse
    :: Int -- ^ 'mcsrsResponseStatus'
    -> ModifyClusterSnapshotResponse
modifyClusterSnapshotResponse pResponseStatus_ =
  ModifyClusterSnapshotResponse'
    {_mcsrsSnapshot = Nothing, _mcsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mcsrsSnapshot :: Lens' ModifyClusterSnapshotResponse (Maybe Snapshot)
mcsrsSnapshot = lens _mcsrsSnapshot (\ s a -> s{_mcsrsSnapshot = a})

-- | -- | The response status code.
mcsrsResponseStatus :: Lens' ModifyClusterSnapshotResponse Int
mcsrsResponseStatus = lens _mcsrsResponseStatus (\ s a -> s{_mcsrsResponseStatus = a})

instance NFData ModifyClusterSnapshotResponse where

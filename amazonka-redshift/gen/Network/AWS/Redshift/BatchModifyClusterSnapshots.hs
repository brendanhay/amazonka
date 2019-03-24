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
-- Module      : Network.AWS.Redshift.BatchModifyClusterSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a list of snapshots.
--
--
module Network.AWS.Redshift.BatchModifyClusterSnapshots
    (
    -- * Creating a Request
      batchModifyClusterSnapshots
    , BatchModifyClusterSnapshots
    -- * Request Lenses
    , bmcsManualSnapshotRetentionPeriod
    , bmcsForce
    , bmcsSnapshotIdentifierList

    -- * Destructuring the Response
    , batchModifyClusterSnapshotsResponse
    , BatchModifyClusterSnapshotsResponse
    -- * Response Lenses
    , bmcsrsResources
    , bmcsrsErrors
    , bmcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchModifyClusterSnapshots' smart constructor.
data BatchModifyClusterSnapshots = BatchModifyClusterSnapshots'
  { _bmcsManualSnapshotRetentionPeriod :: !(Maybe Int)
  , _bmcsForce                         :: !(Maybe Bool)
  , _bmcsSnapshotIdentifierList        :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchModifyClusterSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmcsManualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If you specify the value -1, the manual snapshot is retained indefinitely. The number must be either -1 or an integer between 1 and 3,653. If you decrease the manual snapshot retention period from its current value, existing manual snapshots that fall outside of the new retention period will return an error. If you want to suppress the errors and delete the snapshots, use the force option.
--
-- * 'bmcsForce' - A boolean value indicating whether to override an exception if the retention period has passed.
--
-- * 'bmcsSnapshotIdentifierList' - A list of snapshot identifiers you want to modify.
batchModifyClusterSnapshots
    :: BatchModifyClusterSnapshots
batchModifyClusterSnapshots =
  BatchModifyClusterSnapshots'
    { _bmcsManualSnapshotRetentionPeriod = Nothing
    , _bmcsForce = Nothing
    , _bmcsSnapshotIdentifierList = mempty
    }


-- | The number of days that a manual snapshot is retained. If you specify the value -1, the manual snapshot is retained indefinitely. The number must be either -1 or an integer between 1 and 3,653. If you decrease the manual snapshot retention period from its current value, existing manual snapshots that fall outside of the new retention period will return an error. If you want to suppress the errors and delete the snapshots, use the force option.
bmcsManualSnapshotRetentionPeriod :: Lens' BatchModifyClusterSnapshots (Maybe Int)
bmcsManualSnapshotRetentionPeriod = lens _bmcsManualSnapshotRetentionPeriod (\ s a -> s{_bmcsManualSnapshotRetentionPeriod = a})

-- | A boolean value indicating whether to override an exception if the retention period has passed.
bmcsForce :: Lens' BatchModifyClusterSnapshots (Maybe Bool)
bmcsForce = lens _bmcsForce (\ s a -> s{_bmcsForce = a})

-- | A list of snapshot identifiers you want to modify.
bmcsSnapshotIdentifierList :: Lens' BatchModifyClusterSnapshots [Text]
bmcsSnapshotIdentifierList = lens _bmcsSnapshotIdentifierList (\ s a -> s{_bmcsSnapshotIdentifierList = a}) . _Coerce

instance AWSRequest BatchModifyClusterSnapshots where
        type Rs BatchModifyClusterSnapshots =
             BatchModifyClusterSnapshotsResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "BatchModifyClusterSnapshotsResult"
              (\ s h x ->
                 BatchModifyClusterSnapshotsResponse' <$>
                   (x .@? "Resources" .!@ mempty >>=
                      may (parseXMLList "String"))
                     <*>
                     (x .@? "Errors" .!@ mempty >>=
                        may (parseXMLList "SnapshotErrorMessage"))
                     <*> (pure (fromEnum s)))

instance Hashable BatchModifyClusterSnapshots where

instance NFData BatchModifyClusterSnapshots where

instance ToHeaders BatchModifyClusterSnapshots where
        toHeaders = const mempty

instance ToPath BatchModifyClusterSnapshots where
        toPath = const "/"

instance ToQuery BatchModifyClusterSnapshots where
        toQuery BatchModifyClusterSnapshots'{..}
          = mconcat
              ["Action" =:
                 ("BatchModifyClusterSnapshots" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ManualSnapshotRetentionPeriod" =:
                 _bmcsManualSnapshotRetentionPeriod,
               "Force" =: _bmcsForce,
               "SnapshotIdentifierList" =:
                 toQueryList "String" _bmcsSnapshotIdentifierList]

-- | /See:/ 'batchModifyClusterSnapshotsResponse' smart constructor.
data BatchModifyClusterSnapshotsResponse = BatchModifyClusterSnapshotsResponse'
  { _bmcsrsResources      :: !(Maybe [Text])
  , _bmcsrsErrors         :: !(Maybe [SnapshotErrorMessage])
  , _bmcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchModifyClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmcsrsResources' - A list of the snapshots that were modified.
--
-- * 'bmcsrsErrors' - A list of any errors returned.
--
-- * 'bmcsrsResponseStatus' - -- | The response status code.
batchModifyClusterSnapshotsResponse
    :: Int -- ^ 'bmcsrsResponseStatus'
    -> BatchModifyClusterSnapshotsResponse
batchModifyClusterSnapshotsResponse pResponseStatus_ =
  BatchModifyClusterSnapshotsResponse'
    { _bmcsrsResources = Nothing
    , _bmcsrsErrors = Nothing
    , _bmcsrsResponseStatus = pResponseStatus_
    }


-- | A list of the snapshots that were modified.
bmcsrsResources :: Lens' BatchModifyClusterSnapshotsResponse [Text]
bmcsrsResources = lens _bmcsrsResources (\ s a -> s{_bmcsrsResources = a}) . _Default . _Coerce

-- | A list of any errors returned.
bmcsrsErrors :: Lens' BatchModifyClusterSnapshotsResponse [SnapshotErrorMessage]
bmcsrsErrors = lens _bmcsrsErrors (\ s a -> s{_bmcsrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bmcsrsResponseStatus :: Lens' BatchModifyClusterSnapshotsResponse Int
bmcsrsResponseStatus = lens _bmcsrsResponseStatus (\ s a -> s{_bmcsrsResponseStatus = a})

instance NFData BatchModifyClusterSnapshotsResponse
         where

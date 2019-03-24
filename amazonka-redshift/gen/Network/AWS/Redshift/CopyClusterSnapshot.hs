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
-- Module      : Network.AWS.Redshift.CopyClusterSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified automated cluster snapshot to a new manual cluster snapshot. The source must be an automated snapshot and it must be in the available state.
--
--
-- When you delete a cluster, Amazon Redshift deletes any automated snapshots of the cluster. Also, when the retention period of the snapshot expires, Amazon Redshift automatically deletes it. If you want to keep an automated snapshot for a longer period, you can make a manual copy of the snapshot. Manual snapshots are retained until you delete them.
--
-- For more information about working with snapshots, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.CopyClusterSnapshot
    (
    -- * Creating a Request
      copyClusterSnapshot
    , CopyClusterSnapshot
    -- * Request Lenses
    , copManualSnapshotRetentionPeriod
    , copSourceSnapshotClusterIdentifier
    , copSourceSnapshotIdentifier
    , copTargetSnapshotIdentifier

    -- * Destructuring the Response
    , copyClusterSnapshotResponse
    , CopyClusterSnapshotResponse
    -- * Response Lenses
    , ccsrsSnapshot
    , ccsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'copyClusterSnapshot' smart constructor.
data CopyClusterSnapshot = CopyClusterSnapshot'
  { _copManualSnapshotRetentionPeriod   :: !(Maybe Int)
  , _copSourceSnapshotClusterIdentifier :: !(Maybe Text)
  , _copSourceSnapshotIdentifier        :: !Text
  , _copTargetSnapshotIdentifier        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'copManualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.  The value must be either -1 or an integer between 1 and 3,653. The default value is -1.
--
-- * 'copSourceSnapshotClusterIdentifier' - The identifier of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name. Constraints:     * Must be the identifier for a valid cluster.
--
-- * 'copSourceSnapshotIdentifier' - The identifier for the source snapshot. Constraints:     * Must be the identifier for a valid automated snapshot whose state is @available@ .
--
-- * 'copTargetSnapshotIdentifier' - The identifier given to the new manual snapshot. Constraints:     * Cannot be null, empty, or blank.     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.     * Must be unique for the AWS account that is making the request.
copyClusterSnapshot
    :: Text -- ^ 'copSourceSnapshotIdentifier'
    -> Text -- ^ 'copTargetSnapshotIdentifier'
    -> CopyClusterSnapshot
copyClusterSnapshot pSourceSnapshotIdentifier_ pTargetSnapshotIdentifier_ =
  CopyClusterSnapshot'
    { _copManualSnapshotRetentionPeriod = Nothing
    , _copSourceSnapshotClusterIdentifier = Nothing
    , _copSourceSnapshotIdentifier = pSourceSnapshotIdentifier_
    , _copTargetSnapshotIdentifier = pTargetSnapshotIdentifier_
    }


-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.  The value must be either -1 or an integer between 1 and 3,653. The default value is -1.
copManualSnapshotRetentionPeriod :: Lens' CopyClusterSnapshot (Maybe Int)
copManualSnapshotRetentionPeriod = lens _copManualSnapshotRetentionPeriod (\ s a -> s{_copManualSnapshotRetentionPeriod = a})

-- | The identifier of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name. Constraints:     * Must be the identifier for a valid cluster.
copSourceSnapshotClusterIdentifier :: Lens' CopyClusterSnapshot (Maybe Text)
copSourceSnapshotClusterIdentifier = lens _copSourceSnapshotClusterIdentifier (\ s a -> s{_copSourceSnapshotClusterIdentifier = a})

-- | The identifier for the source snapshot. Constraints:     * Must be the identifier for a valid automated snapshot whose state is @available@ .
copSourceSnapshotIdentifier :: Lens' CopyClusterSnapshot Text
copSourceSnapshotIdentifier = lens _copSourceSnapshotIdentifier (\ s a -> s{_copSourceSnapshotIdentifier = a})

-- | The identifier given to the new manual snapshot. Constraints:     * Cannot be null, empty, or blank.     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.     * Must be unique for the AWS account that is making the request.
copTargetSnapshotIdentifier :: Lens' CopyClusterSnapshot Text
copTargetSnapshotIdentifier = lens _copTargetSnapshotIdentifier (\ s a -> s{_copTargetSnapshotIdentifier = a})

instance AWSRequest CopyClusterSnapshot where
        type Rs CopyClusterSnapshot =
             CopyClusterSnapshotResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "CopyClusterSnapshotResult"
              (\ s h x ->
                 CopyClusterSnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance Hashable CopyClusterSnapshot where

instance NFData CopyClusterSnapshot where

instance ToHeaders CopyClusterSnapshot where
        toHeaders = const mempty

instance ToPath CopyClusterSnapshot where
        toPath = const "/"

instance ToQuery CopyClusterSnapshot where
        toQuery CopyClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyClusterSnapshot" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ManualSnapshotRetentionPeriod" =:
                 _copManualSnapshotRetentionPeriod,
               "SourceSnapshotClusterIdentifier" =:
                 _copSourceSnapshotClusterIdentifier,
               "SourceSnapshotIdentifier" =:
                 _copSourceSnapshotIdentifier,
               "TargetSnapshotIdentifier" =:
                 _copTargetSnapshotIdentifier]

-- | /See:/ 'copyClusterSnapshotResponse' smart constructor.
data CopyClusterSnapshotResponse = CopyClusterSnapshotResponse'
  { _ccsrsSnapshot       :: !(Maybe Snapshot)
  , _ccsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsrsSnapshot' - Undocumented member.
--
-- * 'ccsrsResponseStatus' - -- | The response status code.
copyClusterSnapshotResponse
    :: Int -- ^ 'ccsrsResponseStatus'
    -> CopyClusterSnapshotResponse
copyClusterSnapshotResponse pResponseStatus_ =
  CopyClusterSnapshotResponse'
    {_ccsrsSnapshot = Nothing, _ccsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ccsrsSnapshot :: Lens' CopyClusterSnapshotResponse (Maybe Snapshot)
ccsrsSnapshot = lens _ccsrsSnapshot (\ s a -> s{_ccsrsSnapshot = a})

-- | -- | The response status code.
ccsrsResponseStatus :: Lens' CopyClusterSnapshotResponse Int
ccsrsResponseStatus = lens _ccsrsResponseStatus (\ s a -> s{_ccsrsResponseStatus = a})

instance NFData CopyClusterSnapshotResponse where

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
-- Module      : Network.AWS.Redshift.RestoreTableFromClusterSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table from a table in an Amazon Redshift cluster snapshot. You must create the new table within the Amazon Redshift cluster that the snapshot was taken from.
--
--
-- You cannot use @RestoreTableFromClusterSnapshot@ to restore a table with the same name as an existing table in an Amazon Redshift cluster. That is, you cannot overwrite an existing table in a cluster with a restored table. If you want to replace your original table with a new, restored table, then rename or drop your original table before you call @RestoreTableFromClusterSnapshot@ . When you have renamed your original table, then you can pass the original name of the table as the @NewTableName@ parameter value in the call to @RestoreTableFromClusterSnapshot@ . This way, you can replace the original table with the table created from the snapshot.
--
module Network.AWS.Redshift.RestoreTableFromClusterSnapshot
    (
    -- * Creating a Request
      restoreTableFromClusterSnapshot
    , RestoreTableFromClusterSnapshot
    -- * Request Lenses
    , rtfcsTargetSchemaName
    , rtfcsTargetDatabaseName
    , rtfcsSourceSchemaName
    , rtfcsClusterIdentifier
    , rtfcsSnapshotIdentifier
    , rtfcsSourceDatabaseName
    , rtfcsSourceTableName
    , rtfcsNewTableName

    -- * Destructuring the Response
    , restoreTableFromClusterSnapshotResponse
    , RestoreTableFromClusterSnapshotResponse
    -- * Response Lenses
    , rtfcsrsTableRestoreStatus
    , rtfcsrsResponseStatus
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
-- /See:/ 'restoreTableFromClusterSnapshot' smart constructor.
data RestoreTableFromClusterSnapshot = RestoreTableFromClusterSnapshot'
  { _rtfcsTargetSchemaName   :: !(Maybe Text)
  , _rtfcsTargetDatabaseName :: !(Maybe Text)
  , _rtfcsSourceSchemaName   :: !(Maybe Text)
  , _rtfcsClusterIdentifier  :: !Text
  , _rtfcsSnapshotIdentifier :: !Text
  , _rtfcsSourceDatabaseName :: !Text
  , _rtfcsSourceTableName    :: !Text
  , _rtfcsNewTableName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreTableFromClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfcsTargetSchemaName' - The name of the schema to restore the table to.
--
-- * 'rtfcsTargetDatabaseName' - The name of the database to restore the table to.
--
-- * 'rtfcsSourceSchemaName' - The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
--
-- * 'rtfcsClusterIdentifier' - The identifier of the Amazon Redshift cluster to restore the table to.
--
-- * 'rtfcsSnapshotIdentifier' - The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
--
-- * 'rtfcsSourceDatabaseName' - The name of the source database that contains the table to restore from.
--
-- * 'rtfcsSourceTableName' - The name of the source table to restore from.
--
-- * 'rtfcsNewTableName' - The name of the table to create as a result of the current request.
restoreTableFromClusterSnapshot
    :: Text -- ^ 'rtfcsClusterIdentifier'
    -> Text -- ^ 'rtfcsSnapshotIdentifier'
    -> Text -- ^ 'rtfcsSourceDatabaseName'
    -> Text -- ^ 'rtfcsSourceTableName'
    -> Text -- ^ 'rtfcsNewTableName'
    -> RestoreTableFromClusterSnapshot
restoreTableFromClusterSnapshot pClusterIdentifier_ pSnapshotIdentifier_ pSourceDatabaseName_ pSourceTableName_ pNewTableName_ =
  RestoreTableFromClusterSnapshot'
    { _rtfcsTargetSchemaName = Nothing
    , _rtfcsTargetDatabaseName = Nothing
    , _rtfcsSourceSchemaName = Nothing
    , _rtfcsClusterIdentifier = pClusterIdentifier_
    , _rtfcsSnapshotIdentifier = pSnapshotIdentifier_
    , _rtfcsSourceDatabaseName = pSourceDatabaseName_
    , _rtfcsSourceTableName = pSourceTableName_
    , _rtfcsNewTableName = pNewTableName_
    }


-- | The name of the schema to restore the table to.
rtfcsTargetSchemaName :: Lens' RestoreTableFromClusterSnapshot (Maybe Text)
rtfcsTargetSchemaName = lens _rtfcsTargetSchemaName (\ s a -> s{_rtfcsTargetSchemaName = a})

-- | The name of the database to restore the table to.
rtfcsTargetDatabaseName :: Lens' RestoreTableFromClusterSnapshot (Maybe Text)
rtfcsTargetDatabaseName = lens _rtfcsTargetDatabaseName (\ s a -> s{_rtfcsTargetDatabaseName = a})

-- | The name of the source schema that contains the table to restore from. If you do not specify a @SourceSchemaName@ value, the default is @public@ .
rtfcsSourceSchemaName :: Lens' RestoreTableFromClusterSnapshot (Maybe Text)
rtfcsSourceSchemaName = lens _rtfcsSourceSchemaName (\ s a -> s{_rtfcsSourceSchemaName = a})

-- | The identifier of the Amazon Redshift cluster to restore the table to.
rtfcsClusterIdentifier :: Lens' RestoreTableFromClusterSnapshot Text
rtfcsClusterIdentifier = lens _rtfcsClusterIdentifier (\ s a -> s{_rtfcsClusterIdentifier = a})

-- | The identifier of the snapshot to restore the table from. This snapshot must have been created from the Amazon Redshift cluster specified by the @ClusterIdentifier@ parameter.
rtfcsSnapshotIdentifier :: Lens' RestoreTableFromClusterSnapshot Text
rtfcsSnapshotIdentifier = lens _rtfcsSnapshotIdentifier (\ s a -> s{_rtfcsSnapshotIdentifier = a})

-- | The name of the source database that contains the table to restore from.
rtfcsSourceDatabaseName :: Lens' RestoreTableFromClusterSnapshot Text
rtfcsSourceDatabaseName = lens _rtfcsSourceDatabaseName (\ s a -> s{_rtfcsSourceDatabaseName = a})

-- | The name of the source table to restore from.
rtfcsSourceTableName :: Lens' RestoreTableFromClusterSnapshot Text
rtfcsSourceTableName = lens _rtfcsSourceTableName (\ s a -> s{_rtfcsSourceTableName = a})

-- | The name of the table to create as a result of the current request.
rtfcsNewTableName :: Lens' RestoreTableFromClusterSnapshot Text
rtfcsNewTableName = lens _rtfcsNewTableName (\ s a -> s{_rtfcsNewTableName = a})

instance AWSRequest RestoreTableFromClusterSnapshot
         where
        type Rs RestoreTableFromClusterSnapshot =
             RestoreTableFromClusterSnapshotResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "RestoreTableFromClusterSnapshotResult"
              (\ s h x ->
                 RestoreTableFromClusterSnapshotResponse' <$>
                   (x .@? "TableRestoreStatus") <*> (pure (fromEnum s)))

instance Hashable RestoreTableFromClusterSnapshot
         where

instance NFData RestoreTableFromClusterSnapshot where

instance ToHeaders RestoreTableFromClusterSnapshot
         where
        toHeaders = const mempty

instance ToPath RestoreTableFromClusterSnapshot where
        toPath = const "/"

instance ToQuery RestoreTableFromClusterSnapshot
         where
        toQuery RestoreTableFromClusterSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("RestoreTableFromClusterSnapshot" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TargetSchemaName" =: _rtfcsTargetSchemaName,
               "TargetDatabaseName" =: _rtfcsTargetDatabaseName,
               "SourceSchemaName" =: _rtfcsSourceSchemaName,
               "ClusterIdentifier" =: _rtfcsClusterIdentifier,
               "SnapshotIdentifier" =: _rtfcsSnapshotIdentifier,
               "SourceDatabaseName" =: _rtfcsSourceDatabaseName,
               "SourceTableName" =: _rtfcsSourceTableName,
               "NewTableName" =: _rtfcsNewTableName]

-- | /See:/ 'restoreTableFromClusterSnapshotResponse' smart constructor.
data RestoreTableFromClusterSnapshotResponse = RestoreTableFromClusterSnapshotResponse'
  { _rtfcsrsTableRestoreStatus :: !(Maybe TableRestoreStatus)
  , _rtfcsrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreTableFromClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfcsrsTableRestoreStatus' - Undocumented member.
--
-- * 'rtfcsrsResponseStatus' - -- | The response status code.
restoreTableFromClusterSnapshotResponse
    :: Int -- ^ 'rtfcsrsResponseStatus'
    -> RestoreTableFromClusterSnapshotResponse
restoreTableFromClusterSnapshotResponse pResponseStatus_ =
  RestoreTableFromClusterSnapshotResponse'
    { _rtfcsrsTableRestoreStatus = Nothing
    , _rtfcsrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
rtfcsrsTableRestoreStatus :: Lens' RestoreTableFromClusterSnapshotResponse (Maybe TableRestoreStatus)
rtfcsrsTableRestoreStatus = lens _rtfcsrsTableRestoreStatus (\ s a -> s{_rtfcsrsTableRestoreStatus = a})

-- | -- | The response status code.
rtfcsrsResponseStatus :: Lens' RestoreTableFromClusterSnapshotResponse Int
rtfcsrsResponseStatus = lens _rtfcsrsResponseStatus (\ s a -> s{_rtfcsrsResponseStatus = a})

instance NFData
           RestoreTableFromClusterSnapshotResponse
         where

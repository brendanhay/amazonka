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
-- Module      : Network.AWS.Discovery.StartContinuousExport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the continuous flow of agent's discovered data into Amazon Athena.
--
--
module Network.AWS.Discovery.StartContinuousExport
    (
    -- * Creating a Request
      startContinuousExport
    , StartContinuousExport

    -- * Destructuring the Response
    , startContinuousExportResponse
    , StartContinuousExportResponse
    -- * Response Lenses
    , scersStartTime
    , scersSchemaStorageConfig
    , scersDataSource
    , scersS3Bucket
    , scersExportId
    , scersResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startContinuousExport' smart constructor.
data StartContinuousExport =
  StartContinuousExport'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartContinuousExport' with the minimum fields required to make a request.
--
startContinuousExport
    :: StartContinuousExport
startContinuousExport = StartContinuousExport'


instance AWSRequest StartContinuousExport where
        type Rs StartContinuousExport =
             StartContinuousExportResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 StartContinuousExportResponse' <$>
                   (x .?> "startTime") <*>
                     (x .?> "schemaStorageConfig" .!@ mempty)
                     <*> (x .?> "dataSource")
                     <*> (x .?> "s3Bucket")
                     <*> (x .?> "exportId")
                     <*> (pure (fromEnum s)))

instance Hashable StartContinuousExport where

instance NFData StartContinuousExport where

instance ToHeaders StartContinuousExport where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.StartContinuousExport"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartContinuousExport where
        toJSON = const (Object mempty)

instance ToPath StartContinuousExport where
        toPath = const "/"

instance ToQuery StartContinuousExport where
        toQuery = const mempty

-- | /See:/ 'startContinuousExportResponse' smart constructor.
data StartContinuousExportResponse = StartContinuousExportResponse'
  { _scersStartTime           :: !(Maybe POSIX)
  , _scersSchemaStorageConfig :: !(Maybe (Map Text Text))
  , _scersDataSource          :: !(Maybe DataSource)
  , _scersS3Bucket            :: !(Maybe Text)
  , _scersExportId            :: !(Maybe Text)
  , _scersResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartContinuousExportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scersStartTime' - The timestamp representing when the continuous export was started.
--
-- * 'scersSchemaStorageConfig' - A dictionary which describes how the data is stored.     * @databaseName@ - the name of the Glue database used to store the schema.
--
-- * 'scersDataSource' - The type of data collector used to gather this data (currently only offered for AGENT).
--
-- * 'scersS3Bucket' - The name of the s3 bucket where the export data parquet files are stored.
--
-- * 'scersExportId' - The unique ID assigned to this export.
--
-- * 'scersResponseStatus' - -- | The response status code.
startContinuousExportResponse
    :: Int -- ^ 'scersResponseStatus'
    -> StartContinuousExportResponse
startContinuousExportResponse pResponseStatus_ =
  StartContinuousExportResponse'
    { _scersStartTime = Nothing
    , _scersSchemaStorageConfig = Nothing
    , _scersDataSource = Nothing
    , _scersS3Bucket = Nothing
    , _scersExportId = Nothing
    , _scersResponseStatus = pResponseStatus_
    }


-- | The timestamp representing when the continuous export was started.
scersStartTime :: Lens' StartContinuousExportResponse (Maybe UTCTime)
scersStartTime = lens _scersStartTime (\ s a -> s{_scersStartTime = a}) . mapping _Time

-- | A dictionary which describes how the data is stored.     * @databaseName@ - the name of the Glue database used to store the schema.
scersSchemaStorageConfig :: Lens' StartContinuousExportResponse (HashMap Text Text)
scersSchemaStorageConfig = lens _scersSchemaStorageConfig (\ s a -> s{_scersSchemaStorageConfig = a}) . _Default . _Map

-- | The type of data collector used to gather this data (currently only offered for AGENT).
scersDataSource :: Lens' StartContinuousExportResponse (Maybe DataSource)
scersDataSource = lens _scersDataSource (\ s a -> s{_scersDataSource = a})

-- | The name of the s3 bucket where the export data parquet files are stored.
scersS3Bucket :: Lens' StartContinuousExportResponse (Maybe Text)
scersS3Bucket = lens _scersS3Bucket (\ s a -> s{_scersS3Bucket = a})

-- | The unique ID assigned to this export.
scersExportId :: Lens' StartContinuousExportResponse (Maybe Text)
scersExportId = lens _scersExportId (\ s a -> s{_scersExportId = a})

-- | -- | The response status code.
scersResponseStatus :: Lens' StartContinuousExportResponse Int
scersResponseStatus = lens _scersResponseStatus (\ s a -> s{_scersResponseStatus = a})

instance NFData StartContinuousExportResponse where

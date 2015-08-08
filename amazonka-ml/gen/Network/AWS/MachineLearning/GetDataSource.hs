{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetDataSource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a @DataSource@ that includes metadata and data file information,
-- as well as the current status of the @DataSource@.
--
-- @GetDataSource@ provides results in normal or verbose format. The
-- verbose format adds the schema description and the list of files pointed
-- to by the DataSource to the normal format.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_GetDataSource.html AWS API Reference> for GetDataSource.
module Network.AWS.MachineLearning.GetDataSource
    (
    -- * Creating a Request
      GetDataSource
    , getDataSource
    -- * Request Lenses
    , gdsVerbose
    , gdsDataSourceId

    -- * Destructuring the Response
    , GetDataSourceResponse
    , getDataSourceResponse
    -- * Response Lenses
    , gdsrsNumberOfFiles
    , gdsrsLastUpdatedAt
    , gdsrsCreatedAt
    , gdsrsRDSMetadata
    , gdsrsDataSourceId
    , gdsrsDataSizeInBytes
    , gdsrsDataSourceSchema
    , gdsrsName
    , gdsrsCreatedByIAMUser
    , gdsrsLogURI
    , gdsrsDataLocationS3
    , gdsrsComputeStatistics
    , gdsrsMessage
    , gdsrsRedshiftMetadata
    , gdsrsRoleARN
    , gdsrsDataRearrangement
    , gdsrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getDataSource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdsVerbose'
--
-- * 'gdsDataSourceId'
data GetDataSource = GetDataSource'
    { _gdsVerbose      :: !(Maybe Bool)
    , _gdsDataSourceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDataSource' smart constructor.
getDataSource :: Text -> GetDataSource
getDataSource pDataSourceId_ =
    GetDataSource'
    { _gdsVerbose = Nothing
    , _gdsDataSourceId = pDataSourceId_
    }

-- | Specifies whether the @GetDataSource@ operation should return
-- @DataSourceSchema@.
--
-- If true, @DataSourceSchema@ is returned.
--
-- If false, @DataSourceSchema@ is not returned.
gdsVerbose :: Lens' GetDataSource (Maybe Bool)
gdsVerbose = lens _gdsVerbose (\ s a -> s{_gdsVerbose = a});

-- | The ID assigned to the @DataSource@ at creation.
gdsDataSourceId :: Lens' GetDataSource Text
gdsDataSourceId = lens _gdsDataSourceId (\ s a -> s{_gdsDataSourceId = a});

instance AWSRequest GetDataSource where
        type Sv GetDataSource = MachineLearning
        type Rs GetDataSource = GetDataSourceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDataSourceResponse' <$>
                   (x .?> "NumberOfFiles") <*> (x .?> "LastUpdatedAt")
                     <*> (x .?> "CreatedAt")
                     <*> (x .?> "RDSMetadata")
                     <*> (x .?> "DataSourceId")
                     <*> (x .?> "DataSizeInBytes")
                     <*> (x .?> "DataSourceSchema")
                     <*> (x .?> "Name")
                     <*> (x .?> "CreatedByIamUser")
                     <*> (x .?> "LogUri")
                     <*> (x .?> "DataLocationS3")
                     <*> (x .?> "ComputeStatistics")
                     <*> (x .?> "Message")
                     <*> (x .?> "RedshiftMetadata")
                     <*> (x .?> "RoleARN")
                     <*> (x .?> "DataRearrangement")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetDataSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.GetDataSource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDataSource where
        toJSON GetDataSource'{..}
          = object
              ["Verbose" .= _gdsVerbose,
               "DataSourceId" .= _gdsDataSourceId]

instance ToPath GetDataSource where
        toPath = const "/"

instance ToQuery GetDataSource where
        toQuery = const mempty

-- | Represents the output of a GetDataSource operation and describes a
-- @DataSource@.
--
-- /See:/ 'getDataSourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdsrsNumberOfFiles'
--
-- * 'gdsrsLastUpdatedAt'
--
-- * 'gdsrsCreatedAt'
--
-- * 'gdsrsRDSMetadata'
--
-- * 'gdsrsDataSourceId'
--
-- * 'gdsrsDataSizeInBytes'
--
-- * 'gdsrsDataSourceSchema'
--
-- * 'gdsrsName'
--
-- * 'gdsrsCreatedByIAMUser'
--
-- * 'gdsrsLogURI'
--
-- * 'gdsrsDataLocationS3'
--
-- * 'gdsrsComputeStatistics'
--
-- * 'gdsrsMessage'
--
-- * 'gdsrsRedshiftMetadata'
--
-- * 'gdsrsRoleARN'
--
-- * 'gdsrsDataRearrangement'
--
-- * 'gdsrsStatus'
data GetDataSourceResponse = GetDataSourceResponse'
    { _gdsrsNumberOfFiles     :: !(Maybe Integer)
    , _gdsrsLastUpdatedAt     :: !(Maybe POSIX)
    , _gdsrsCreatedAt         :: !(Maybe POSIX)
    , _gdsrsRDSMetadata       :: !(Maybe RDSMetadata)
    , _gdsrsDataSourceId      :: !(Maybe Text)
    , _gdsrsDataSizeInBytes   :: !(Maybe Integer)
    , _gdsrsDataSourceSchema  :: !(Maybe Text)
    , _gdsrsName              :: !(Maybe Text)
    , _gdsrsCreatedByIAMUser  :: !(Maybe Text)
    , _gdsrsLogURI            :: !(Maybe Text)
    , _gdsrsDataLocationS3    :: !(Maybe Text)
    , _gdsrsComputeStatistics :: !(Maybe Bool)
    , _gdsrsMessage           :: !(Maybe Text)
    , _gdsrsRedshiftMetadata  :: !(Maybe RedshiftMetadata)
    , _gdsrsRoleARN           :: !(Maybe Text)
    , _gdsrsDataRearrangement :: !(Maybe Text)
    , _gdsrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDataSourceResponse' smart constructor.
getDataSourceResponse :: Int -> GetDataSourceResponse
getDataSourceResponse pStatus_ =
    GetDataSourceResponse'
    { _gdsrsNumberOfFiles = Nothing
    , _gdsrsLastUpdatedAt = Nothing
    , _gdsrsCreatedAt = Nothing
    , _gdsrsRDSMetadata = Nothing
    , _gdsrsDataSourceId = Nothing
    , _gdsrsDataSizeInBytes = Nothing
    , _gdsrsDataSourceSchema = Nothing
    , _gdsrsName = Nothing
    , _gdsrsCreatedByIAMUser = Nothing
    , _gdsrsLogURI = Nothing
    , _gdsrsDataLocationS3 = Nothing
    , _gdsrsComputeStatistics = Nothing
    , _gdsrsMessage = Nothing
    , _gdsrsRedshiftMetadata = Nothing
    , _gdsrsRoleARN = Nothing
    , _gdsrsDataRearrangement = Nothing
    , _gdsrsStatus = pStatus_
    }

-- | The number of data files referenced by the @DataSource@.
gdsrsNumberOfFiles :: Lens' GetDataSourceResponse (Maybe Integer)
gdsrsNumberOfFiles = lens _gdsrsNumberOfFiles (\ s a -> s{_gdsrsNumberOfFiles = a});

-- | The time of the most recent edit to the @DataSource@. The time is
-- expressed in epoch time.
gdsrsLastUpdatedAt :: Lens' GetDataSourceResponse (Maybe UTCTime)
gdsrsLastUpdatedAt = lens _gdsrsLastUpdatedAt (\ s a -> s{_gdsrsLastUpdatedAt = a}) . mapping _Time;

-- | The time that the @DataSource@ was created. The time is expressed in
-- epoch time.
gdsrsCreatedAt :: Lens' GetDataSourceResponse (Maybe UTCTime)
gdsrsCreatedAt = lens _gdsrsCreatedAt (\ s a -> s{_gdsrsCreatedAt = a}) . mapping _Time;

-- | Undocumented member.
gdsrsRDSMetadata :: Lens' GetDataSourceResponse (Maybe RDSMetadata)
gdsrsRDSMetadata = lens _gdsrsRDSMetadata (\ s a -> s{_gdsrsRDSMetadata = a});

-- | The ID assigned to the @DataSource@ at creation. This value should be
-- identical to the value of the @DataSourceId@ in the request.
gdsrsDataSourceId :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsDataSourceId = lens _gdsrsDataSourceId (\ s a -> s{_gdsrsDataSourceId = a});

-- | The total size of observations in the data files.
gdsrsDataSizeInBytes :: Lens' GetDataSourceResponse (Maybe Integer)
gdsrsDataSizeInBytes = lens _gdsrsDataSizeInBytes (\ s a -> s{_gdsrsDataSizeInBytes = a});

-- | The schema used by all of the data files of this @DataSource@.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
gdsrsDataSourceSchema :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsDataSourceSchema = lens _gdsrsDataSourceSchema (\ s a -> s{_gdsrsDataSourceSchema = a});

-- | A user-supplied name or description of the @DataSource@.
gdsrsName :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsName = lens _gdsrsName (\ s a -> s{_gdsrsName = a});

-- | The AWS user account from which the @DataSource@ was created. The
-- account type can be either an AWS root account or an AWS Identity and
-- Access Management (IAM) user account.
gdsrsCreatedByIAMUser :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsCreatedByIAMUser = lens _gdsrsCreatedByIAMUser (\ s a -> s{_gdsrsCreatedByIAMUser = a});

-- | A link to the file containining logs of either create @DataSource@
-- operation.
gdsrsLogURI :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsLogURI = lens _gdsrsLogURI (\ s a -> s{_gdsrsLogURI = a});

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
gdsrsDataLocationS3 :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsDataLocationS3 = lens _gdsrsDataLocationS3 (\ s a -> s{_gdsrsDataLocationS3 = a});

-- | The parameter is @true@ if statistics need to be generated from the
-- observation data.
gdsrsComputeStatistics :: Lens' GetDataSourceResponse (Maybe Bool)
gdsrsComputeStatistics = lens _gdsrsComputeStatistics (\ s a -> s{_gdsrsComputeStatistics = a});

-- | The description of the most recent details about creating the
-- @DataSource@.
gdsrsMessage :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsMessage = lens _gdsrsMessage (\ s a -> s{_gdsrsMessage = a});

-- | Undocumented member.
gdsrsRedshiftMetadata :: Lens' GetDataSourceResponse (Maybe RedshiftMetadata)
gdsrsRedshiftMetadata = lens _gdsrsRedshiftMetadata (\ s a -> s{_gdsrsRedshiftMetadata = a});

-- | Undocumented member.
gdsrsRoleARN :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsRoleARN = lens _gdsrsRoleARN (\ s a -> s{_gdsrsRoleARN = a});

-- | A JSON string that captures the splitting rearrangement requirement of
-- the @DataSource@.
gdsrsDataRearrangement :: Lens' GetDataSourceResponse (Maybe Text)
gdsrsDataRearrangement = lens _gdsrsDataRearrangement (\ s a -> s{_gdsrsDataRearrangement = a});

-- | Undocumented member.
gdsrsStatus :: Lens' GetDataSourceResponse Int
gdsrsStatus = lens _gdsrsStatus (\ s a -> s{_gdsrsStatus = a});

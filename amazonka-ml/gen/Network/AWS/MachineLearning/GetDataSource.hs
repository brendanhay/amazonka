{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.GetDataSource
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a @DataSource@ that includes metadata and data file information,
-- as well as the current status of the @DataSource@.
--
-- @GetDataSource@ provides results in normal or verbose format. The
-- verbose format adds the schema description and the list of files pointed
-- to by the DataSource to the normal format.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_GetDataSource.html>
module Network.AWS.MachineLearning.GetDataSource
    (
    -- * Request
      GetDataSource
    -- ** Request constructor
    , getDataSource
    -- ** Request lenses
    , gdsVerbose
    , gdsDataSourceId

    -- * Response
    , GetDataSourceResponse
    -- ** Response constructor
    , getDataSourceResponse
    -- ** Response lenses
    , gdsrNumberOfFiles
    , gdsrLastUpdatedAt
    , gdsrCreatedAt
    , gdsrRDSMetadata
    , gdsrDataSourceId
    , gdsrDataSizeInBytes
    , gdsrDataSourceSchema
    , gdsrName
    , gdsrCreatedByIAMUser
    , gdsrLogURI
    , gdsrDataLocationS3
    , gdsrComputeStatistics
    , gdsrMessage
    , gdsrRedshiftMetadata
    , gdsrRoleARN
    , gdsrDataRearrangement
    , gdsrStatus
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
    } deriving (Eq,Read,Show)

-- | 'GetDataSource' smart constructor.
getDataSource :: Text -> GetDataSource
getDataSource pDataSourceId =
    GetDataSource'
    { _gdsVerbose = Nothing
    , _gdsDataSourceId = pDataSourceId
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
                     <*> (pure s))

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
-- * 'gdsrNumberOfFiles'
--
-- * 'gdsrLastUpdatedAt'
--
-- * 'gdsrCreatedAt'
--
-- * 'gdsrRDSMetadata'
--
-- * 'gdsrDataSourceId'
--
-- * 'gdsrDataSizeInBytes'
--
-- * 'gdsrDataSourceSchema'
--
-- * 'gdsrName'
--
-- * 'gdsrCreatedByIAMUser'
--
-- * 'gdsrLogURI'
--
-- * 'gdsrDataLocationS3'
--
-- * 'gdsrComputeStatistics'
--
-- * 'gdsrMessage'
--
-- * 'gdsrRedshiftMetadata'
--
-- * 'gdsrRoleARN'
--
-- * 'gdsrDataRearrangement'
--
-- * 'gdsrStatus'
data GetDataSourceResponse = GetDataSourceResponse'
    { _gdsrNumberOfFiles     :: !(Maybe Integer)
    , _gdsrLastUpdatedAt     :: !(Maybe POSIX)
    , _gdsrCreatedAt         :: !(Maybe POSIX)
    , _gdsrRDSMetadata       :: !(Maybe RDSMetadata)
    , _gdsrDataSourceId      :: !(Maybe Text)
    , _gdsrDataSizeInBytes   :: !(Maybe Integer)
    , _gdsrDataSourceSchema  :: !(Maybe Text)
    , _gdsrName              :: !(Maybe Text)
    , _gdsrCreatedByIAMUser  :: !(Maybe Text)
    , _gdsrLogURI            :: !(Maybe Text)
    , _gdsrDataLocationS3    :: !(Maybe Text)
    , _gdsrComputeStatistics :: !(Maybe Bool)
    , _gdsrMessage           :: !(Maybe Text)
    , _gdsrRedshiftMetadata  :: !(Maybe RedshiftMetadata)
    , _gdsrRoleARN           :: !(Maybe Text)
    , _gdsrDataRearrangement :: !(Maybe Text)
    , _gdsrStatus            :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetDataSourceResponse' smart constructor.
getDataSourceResponse :: Status -> GetDataSourceResponse
getDataSourceResponse pStatus =
    GetDataSourceResponse'
    { _gdsrNumberOfFiles = Nothing
    , _gdsrLastUpdatedAt = Nothing
    , _gdsrCreatedAt = Nothing
    , _gdsrRDSMetadata = Nothing
    , _gdsrDataSourceId = Nothing
    , _gdsrDataSizeInBytes = Nothing
    , _gdsrDataSourceSchema = Nothing
    , _gdsrName = Nothing
    , _gdsrCreatedByIAMUser = Nothing
    , _gdsrLogURI = Nothing
    , _gdsrDataLocationS3 = Nothing
    , _gdsrComputeStatistics = Nothing
    , _gdsrMessage = Nothing
    , _gdsrRedshiftMetadata = Nothing
    , _gdsrRoleARN = Nothing
    , _gdsrDataRearrangement = Nothing
    , _gdsrStatus = pStatus
    }

-- | The number of data files referenced by the @DataSource@.
gdsrNumberOfFiles :: Lens' GetDataSourceResponse (Maybe Integer)
gdsrNumberOfFiles = lens _gdsrNumberOfFiles (\ s a -> s{_gdsrNumberOfFiles = a});

-- | The time of the most recent edit to the @DataSource@. The time is
-- expressed in epoch time.
gdsrLastUpdatedAt :: Lens' GetDataSourceResponse (Maybe UTCTime)
gdsrLastUpdatedAt = lens _gdsrLastUpdatedAt (\ s a -> s{_gdsrLastUpdatedAt = a}) . mapping _Time;

-- | The time that the @DataSource@ was created. The time is expressed in
-- epoch time.
gdsrCreatedAt :: Lens' GetDataSourceResponse (Maybe UTCTime)
gdsrCreatedAt = lens _gdsrCreatedAt (\ s a -> s{_gdsrCreatedAt = a}) . mapping _Time;

-- | FIXME: Undocumented member.
gdsrRDSMetadata :: Lens' GetDataSourceResponse (Maybe RDSMetadata)
gdsrRDSMetadata = lens _gdsrRDSMetadata (\ s a -> s{_gdsrRDSMetadata = a});

-- | The ID assigned to the @DataSource@ at creation. This value should be
-- identical to the value of the @DataSourceId@ in the request.
gdsrDataSourceId :: Lens' GetDataSourceResponse (Maybe Text)
gdsrDataSourceId = lens _gdsrDataSourceId (\ s a -> s{_gdsrDataSourceId = a});

-- | The total size of observations in the data files.
gdsrDataSizeInBytes :: Lens' GetDataSourceResponse (Maybe Integer)
gdsrDataSizeInBytes = lens _gdsrDataSizeInBytes (\ s a -> s{_gdsrDataSizeInBytes = a});

-- | The schema used by all of the data files of this @DataSource@.
--
-- Note
--
-- This parameter is provided as part of the verbose format.
gdsrDataSourceSchema :: Lens' GetDataSourceResponse (Maybe Text)
gdsrDataSourceSchema = lens _gdsrDataSourceSchema (\ s a -> s{_gdsrDataSourceSchema = a});

-- | A user-supplied name or description of the @DataSource@.
gdsrName :: Lens' GetDataSourceResponse (Maybe Text)
gdsrName = lens _gdsrName (\ s a -> s{_gdsrName = a});

-- | The AWS user account from which the @DataSource@ was created. The
-- account type can be either an AWS root account or an AWS Identity and
-- Access Management (IAM) user account.
gdsrCreatedByIAMUser :: Lens' GetDataSourceResponse (Maybe Text)
gdsrCreatedByIAMUser = lens _gdsrCreatedByIAMUser (\ s a -> s{_gdsrCreatedByIAMUser = a});

-- | A link to the file containining logs of either create @DataSource@
-- operation.
gdsrLogURI :: Lens' GetDataSourceResponse (Maybe Text)
gdsrLogURI = lens _gdsrLogURI (\ s a -> s{_gdsrLogURI = a});

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
gdsrDataLocationS3 :: Lens' GetDataSourceResponse (Maybe Text)
gdsrDataLocationS3 = lens _gdsrDataLocationS3 (\ s a -> s{_gdsrDataLocationS3 = a});

-- | The parameter is @true@ if statistics need to be generated from the
-- observation data.
gdsrComputeStatistics :: Lens' GetDataSourceResponse (Maybe Bool)
gdsrComputeStatistics = lens _gdsrComputeStatistics (\ s a -> s{_gdsrComputeStatistics = a});

-- | The description of the most recent details about creating the
-- @DataSource@.
gdsrMessage :: Lens' GetDataSourceResponse (Maybe Text)
gdsrMessage = lens _gdsrMessage (\ s a -> s{_gdsrMessage = a});

-- | FIXME: Undocumented member.
gdsrRedshiftMetadata :: Lens' GetDataSourceResponse (Maybe RedshiftMetadata)
gdsrRedshiftMetadata = lens _gdsrRedshiftMetadata (\ s a -> s{_gdsrRedshiftMetadata = a});

-- | FIXME: Undocumented member.
gdsrRoleARN :: Lens' GetDataSourceResponse (Maybe Text)
gdsrRoleARN = lens _gdsrRoleARN (\ s a -> s{_gdsrRoleARN = a});

-- | A JSON string that captures the splitting rearrangement requirement of
-- the @DataSource@.
gdsrDataRearrangement :: Lens' GetDataSourceResponse (Maybe Text)
gdsrDataRearrangement = lens _gdsrDataRearrangement (\ s a -> s{_gdsrDataRearrangement = a});

-- | FIXME: Undocumented member.
gdsrStatus :: Lens' GetDataSourceResponse Status
gdsrStatus = lens _gdsrStatus (\ s a -> s{_gdsrStatus = a});

{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.MachineLearning.GetDataSource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a 'DataSource' that includes metadata and data file information, as
-- well as the current status of the 'DataSource'.
--
-- 'GetDataSource' provides results in normal or verbose format. The verbose
-- format adds the schema description and the list of files pointed to by the
-- DataSource to the normal format.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_GetDataSource.html>
module Network.AWS.MachineLearning.GetDataSource
    (
    -- * Request
      GetDataSource
    -- ** Request constructor
    , getDataSource
    -- ** Request lenses
    , gdsDataSourceId
    , gdsVerbose

    -- * Response
    , GetDataSourceResponse
    -- ** Response constructor
    , getDataSourceResponse
    -- ** Response lenses
    , gdsrComputeStatistics
    , gdsrCreatedAt
    , gdsrCreatedByIamUser
    , gdsrDataLocationS3
    , gdsrDataRearrangement
    , gdsrDataSizeInBytes
    , gdsrDataSourceId
    , gdsrDataSourceSchema
    , gdsrLastUpdatedAt
    , gdsrLogUri
    , gdsrMessage
    , gdsrName
    , gdsrNumberOfFiles
    , gdsrRDSMetadata
    , gdsrRedshiftMetadata
    , gdsrRoleARN
    , gdsrStatus
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data GetDataSource = GetDataSource
    { _gdsDataSourceId :: Text
    , _gdsVerbose      :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'GetDataSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdsDataSourceId' @::@ 'Text'
--
-- * 'gdsVerbose' @::@ 'Maybe' 'Bool'
--
getDataSource :: Text -- ^ 'gdsDataSourceId'
              -> GetDataSource
getDataSource p1 = GetDataSource
    { _gdsDataSourceId = p1
    , _gdsVerbose      = Nothing
    }

-- | The ID assigned to the 'DataSource' at creation.
gdsDataSourceId :: Lens' GetDataSource Text
gdsDataSourceId = lens _gdsDataSourceId (\s a -> s { _gdsDataSourceId = a })

-- | Specifies whether the 'GetDataSource' operation should return 'DataSourceSchema'.
--
-- If true, 'DataSourceSchema' is returned.
--
-- If false, 'DataSourceSchema' is not returned.
gdsVerbose :: Lens' GetDataSource (Maybe Bool)
gdsVerbose = lens _gdsVerbose (\s a -> s { _gdsVerbose = a })

data GetDataSourceResponse = GetDataSourceResponse
    { _gdsrComputeStatistics :: Maybe Bool
    , _gdsrCreatedAt         :: Maybe POSIX
    , _gdsrCreatedByIamUser  :: Maybe Text
    , _gdsrDataLocationS3    :: Maybe Text
    , _gdsrDataRearrangement :: Maybe Text
    , _gdsrDataSizeInBytes   :: Maybe Integer
    , _gdsrDataSourceId      :: Maybe Text
    , _gdsrDataSourceSchema  :: Maybe Text
    , _gdsrLastUpdatedAt     :: Maybe POSIX
    , _gdsrLogUri            :: Maybe Text
    , _gdsrMessage           :: Maybe Text
    , _gdsrName              :: Maybe Text
    , _gdsrNumberOfFiles     :: Maybe Integer
    , _gdsrRDSMetadata       :: Maybe RDSMetadata
    , _gdsrRedshiftMetadata  :: Maybe RedshiftMetadata
    , _gdsrRoleARN           :: Maybe Text
    , _gdsrStatus            :: Maybe EntityStatus
    } deriving (Eq, Read, Show)

-- | 'GetDataSourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdsrComputeStatistics' @::@ 'Maybe' 'Bool'
--
-- * 'gdsrCreatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gdsrCreatedByIamUser' @::@ 'Maybe' 'Text'
--
-- * 'gdsrDataLocationS3' @::@ 'Maybe' 'Text'
--
-- * 'gdsrDataRearrangement' @::@ 'Maybe' 'Text'
--
-- * 'gdsrDataSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'gdsrDataSourceId' @::@ 'Maybe' 'Text'
--
-- * 'gdsrDataSourceSchema' @::@ 'Maybe' 'Text'
--
-- * 'gdsrLastUpdatedAt' @::@ 'Maybe' 'UTCTime'
--
-- * 'gdsrLogUri' @::@ 'Maybe' 'Text'
--
-- * 'gdsrMessage' @::@ 'Maybe' 'Text'
--
-- * 'gdsrName' @::@ 'Maybe' 'Text'
--
-- * 'gdsrNumberOfFiles' @::@ 'Maybe' 'Integer'
--
-- * 'gdsrRDSMetadata' @::@ 'Maybe' 'RDSMetadata'
--
-- * 'gdsrRedshiftMetadata' @::@ 'Maybe' 'RedshiftMetadata'
--
-- * 'gdsrRoleARN' @::@ 'Maybe' 'Text'
--
-- * 'gdsrStatus' @::@ 'Maybe' 'EntityStatus'
--
getDataSourceResponse :: GetDataSourceResponse
getDataSourceResponse = GetDataSourceResponse
    { _gdsrDataSourceId      = Nothing
    , _gdsrDataLocationS3    = Nothing
    , _gdsrDataRearrangement = Nothing
    , _gdsrCreatedByIamUser  = Nothing
    , _gdsrCreatedAt         = Nothing
    , _gdsrLastUpdatedAt     = Nothing
    , _gdsrDataSizeInBytes   = Nothing
    , _gdsrNumberOfFiles     = Nothing
    , _gdsrName              = Nothing
    , _gdsrStatus            = Nothing
    , _gdsrLogUri            = Nothing
    , _gdsrMessage           = Nothing
    , _gdsrRedshiftMetadata  = Nothing
    , _gdsrRDSMetadata       = Nothing
    , _gdsrRoleARN           = Nothing
    , _gdsrComputeStatistics = Nothing
    , _gdsrDataSourceSchema  = Nothing
    }

-- | The parameter is 'true' if statistics need to be generated from the
-- observation data.
gdsrComputeStatistics :: Lens' GetDataSourceResponse (Maybe Bool)
gdsrComputeStatistics =
    lens _gdsrComputeStatistics (\s a -> s { _gdsrComputeStatistics = a })

-- | The time that the 'DataSource' was created. The time is expressed in epoch time.
gdsrCreatedAt :: Lens' GetDataSourceResponse (Maybe UTCTime)
gdsrCreatedAt = lens _gdsrCreatedAt (\s a -> s { _gdsrCreatedAt = a }) . mapping _Time

-- | The AWS user account from which the 'DataSource' was created. The account type
-- can be either an AWS root account or an AWS Identity and Access Management
-- (IAM) user account.
gdsrCreatedByIamUser :: Lens' GetDataSourceResponse (Maybe Text)
gdsrCreatedByIamUser =
    lens _gdsrCreatedByIamUser (\s a -> s { _gdsrCreatedByIamUser = a })

-- | The location of the data file or directory in Amazon Simple Storage Service
-- (Amazon S3).
gdsrDataLocationS3 :: Lens' GetDataSourceResponse (Maybe Text)
gdsrDataLocationS3 =
    lens _gdsrDataLocationS3 (\s a -> s { _gdsrDataLocationS3 = a })

-- | A JSON string that captures the splitting rearrangement requirement of the 'DataSource'.
gdsrDataRearrangement :: Lens' GetDataSourceResponse (Maybe Text)
gdsrDataRearrangement =
    lens _gdsrDataRearrangement (\s a -> s { _gdsrDataRearrangement = a })

-- | The total size of observations in the data files.
gdsrDataSizeInBytes :: Lens' GetDataSourceResponse (Maybe Integer)
gdsrDataSizeInBytes =
    lens _gdsrDataSizeInBytes (\s a -> s { _gdsrDataSizeInBytes = a })

-- | The ID assigned to the 'DataSource' at creation. This value should be identical
-- to the value of the 'DataSourceId' in the request.
gdsrDataSourceId :: Lens' GetDataSourceResponse (Maybe Text)
gdsrDataSourceId = lens _gdsrDataSourceId (\s a -> s { _gdsrDataSourceId = a })

-- | The schema used by all of the data files of this 'DataSource'.
--
-- Note This parameter is provided as part of the verbose format.
--
gdsrDataSourceSchema :: Lens' GetDataSourceResponse (Maybe Text)
gdsrDataSourceSchema =
    lens _gdsrDataSourceSchema (\s a -> s { _gdsrDataSourceSchema = a })

-- | The time of the most recent edit to the 'DataSource'. The time is expressed in
-- epoch time.
gdsrLastUpdatedAt :: Lens' GetDataSourceResponse (Maybe UTCTime)
gdsrLastUpdatedAt =
    lens _gdsrLastUpdatedAt (\s a -> s { _gdsrLastUpdatedAt = a })
        . mapping _Time

-- | A link to the file containining logs of either create 'DataSource' operation.
gdsrLogUri :: Lens' GetDataSourceResponse (Maybe Text)
gdsrLogUri = lens _gdsrLogUri (\s a -> s { _gdsrLogUri = a })

-- | The description of the most recent details about creating the 'DataSource'.
gdsrMessage :: Lens' GetDataSourceResponse (Maybe Text)
gdsrMessage = lens _gdsrMessage (\s a -> s { _gdsrMessage = a })

-- | A user-supplied name or description of the 'DataSource'.
gdsrName :: Lens' GetDataSourceResponse (Maybe Text)
gdsrName = lens _gdsrName (\s a -> s { _gdsrName = a })

-- | The number of data files referenced by the 'DataSource'.
gdsrNumberOfFiles :: Lens' GetDataSourceResponse (Maybe Integer)
gdsrNumberOfFiles =
    lens _gdsrNumberOfFiles (\s a -> s { _gdsrNumberOfFiles = a })

gdsrRDSMetadata :: Lens' GetDataSourceResponse (Maybe RDSMetadata)
gdsrRDSMetadata = lens _gdsrRDSMetadata (\s a -> s { _gdsrRDSMetadata = a })

gdsrRedshiftMetadata :: Lens' GetDataSourceResponse (Maybe RedshiftMetadata)
gdsrRedshiftMetadata =
    lens _gdsrRedshiftMetadata (\s a -> s { _gdsrRedshiftMetadata = a })

gdsrRoleARN :: Lens' GetDataSourceResponse (Maybe Text)
gdsrRoleARN = lens _gdsrRoleARN (\s a -> s { _gdsrRoleARN = a })

-- | The current status of the 'DataSource'. This element can have one of the
-- following values:
--
-- 'PENDING' - Amazon Machine Language (Amazon ML) submitted a request to
-- create a 'DataSource'.  'INPROGRESS' - The creation process is underway.  'FAILED'
-- - The request to create a 'DataSource' did not run to completion. It is not
-- usable.  'COMPLETED' - The creation process completed successfully.  'DELETED' -
-- The 'DataSource' is marked as deleted. It is not usable.
gdsrStatus :: Lens' GetDataSourceResponse (Maybe EntityStatus)
gdsrStatus = lens _gdsrStatus (\s a -> s { _gdsrStatus = a })

instance ToPath GetDataSource where
    toPath = const "/"

instance ToQuery GetDataSource where
    toQuery = const mempty

instance ToHeaders GetDataSource

instance ToJSON GetDataSource where
    toJSON GetDataSource{..} = object
        [ "DataSourceId" .= _gdsDataSourceId
        , "Verbose"      .= _gdsVerbose
        ]

instance AWSRequest GetDataSource where
    type Sv GetDataSource = MachineLearning
    type Rs GetDataSource = GetDataSourceResponse

    request  = post "GetDataSource"
    response = jsonResponse

instance FromJSON GetDataSourceResponse where
    parseJSON = withObject "GetDataSourceResponse" $ \o -> GetDataSourceResponse
        <$> o .:? "ComputeStatistics"
        <*> o .:? "CreatedAt"
        <*> o .:? "CreatedByIamUser"
        <*> o .:? "DataLocationS3"
        <*> o .:? "DataRearrangement"
        <*> o .:? "DataSizeInBytes"
        <*> o .:? "DataSourceId"
        <*> o .:? "DataSourceSchema"
        <*> o .:? "LastUpdatedAt"
        <*> o .:? "LogUri"
        <*> o .:? "Message"
        <*> o .:? "Name"
        <*> o .:? "NumberOfFiles"
        <*> o .:? "RDSMetadata"
        <*> o .:? "RedshiftMetadata"
        <*> o .:? "RoleARN"
        <*> o .:? "Status"

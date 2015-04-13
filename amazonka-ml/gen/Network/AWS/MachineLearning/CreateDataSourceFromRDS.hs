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

-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRDS
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

-- | Creates a 'DataSource' object from an <http://aws.amazon.com/rds/  Amazon Relational Database Service>
-- (Amazon RDS). A 'DataSource' references data that can be used to perform 'CreateMLModel', 'CreateEvaluation', or 'CreateBatchPrediction' operations.
--
-- 'CreateDataSourceFromRDS' is an asynchronous operation. In response to 'CreateDataSourceFromRDS', Amazon Machine Learning (Amazon ML) immediately returns and sets the 'DataSource' status to 'PENDING'. After the 'DataSource' is created and ready for use, Amazon
-- ML sets the 'Status' parameter to 'COMPLETED'. 'DataSource' in 'COMPLETED' or 'PENDING'
-- status can only be used to perform 'CreateMLModel', 'CreateEvaluation', or 'CreateBatchPrediction' operations.
--
-- If Amazon ML cannot accept the input source, it sets the 'Status' parameter
-- to 'FAILED' and includes an error message in the 'Message' attribute of the 'GetDataSource' operation response.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateDataSourceFromRDS.html>
module Network.AWS.MachineLearning.CreateDataSourceFromRDS
    (
    -- * Request
      CreateDataSourceFromRDS
    -- ** Request constructor
    , createDataSourceFromRDS
    -- ** Request lenses
    , cdsfrdsComputeStatistics
    , cdsfrdsDataSourceId
    , cdsfrdsDataSourceName
    , cdsfrdsRDSData
    , cdsfrdsRoleARN

    -- * Response
    , CreateDataSourceFromRDSResponse
    -- ** Response constructor
    , createDataSourceFromRDSResponse
    -- ** Response lenses
    , cdsfrdsrDataSourceId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data CreateDataSourceFromRDS = CreateDataSourceFromRDS
    { _cdsfrdsComputeStatistics :: Maybe Bool
    , _cdsfrdsDataSourceId      :: Text
    , _cdsfrdsDataSourceName    :: Maybe Text
    , _cdsfrdsRDSData           :: RDSDataSpec
    , _cdsfrdsRoleARN           :: Text
    } deriving (Eq, Read, Show)

-- | 'CreateDataSourceFromRDS' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfrdsComputeStatistics' @::@ 'Maybe' 'Bool'
--
-- * 'cdsfrdsDataSourceId' @::@ 'Text'
--
-- * 'cdsfrdsDataSourceName' @::@ 'Maybe' 'Text'
--
-- * 'cdsfrdsRDSData' @::@ 'RDSDataSpec'
--
-- * 'cdsfrdsRoleARN' @::@ 'Text'
--
createDataSourceFromRDS :: Text -- ^ 'cdsfrdsDataSourceId'
                        -> RDSDataSpec -- ^ 'cdsfrdsRDSData'
                        -> Text -- ^ 'cdsfrdsRoleARN'
                        -> CreateDataSourceFromRDS
createDataSourceFromRDS p1 p2 p3 = CreateDataSourceFromRDS
    { _cdsfrdsDataSourceId      = p1
    , _cdsfrdsRDSData           = p2
    , _cdsfrdsRoleARN           = p3
    , _cdsfrdsDataSourceName    = Nothing
    , _cdsfrdsComputeStatistics = Nothing
    }

-- | The compute statistics for a 'DataSource'. The statistics are generated from
-- the observation data referenced by a 'DataSource'. Amazon ML uses the
-- statistics internally during an 'MLModel' training. This parameter must be set
-- to 'true' if the ''DataSource'' needs to be used for 'MLModel' training.
cdsfrdsComputeStatistics :: Lens' CreateDataSourceFromRDS (Maybe Bool)
cdsfrdsComputeStatistics =
    lens _cdsfrdsComputeStatistics
        (\s a -> s { _cdsfrdsComputeStatistics = a })

-- | A user-supplied ID that uniquely identifies the 'DataSource'. Typically, an
-- Amazon Resource Number (ARN) becomes the ID for a 'DataSource'.
cdsfrdsDataSourceId :: Lens' CreateDataSourceFromRDS Text
cdsfrdsDataSourceId =
    lens _cdsfrdsDataSourceId (\s a -> s { _cdsfrdsDataSourceId = a })

-- | A user-supplied name or description of the 'DataSource'.
cdsfrdsDataSourceName :: Lens' CreateDataSourceFromRDS (Maybe Text)
cdsfrdsDataSourceName =
    lens _cdsfrdsDataSourceName (\s a -> s { _cdsfrdsDataSourceName = a })

-- | The data specification of an Amazon RDS 'DataSource':
--
-- DatabaseInformation -   'DatabaseName ' - Name of the Amazon RDS database.  'InstanceIdentifier ' - Unique identifier for the Amazon RDS database instance.
--
--
-- DatabaseCredentials - AWS Identity and Access Management (IAM) credentials
-- that are used to connect to the Amazon RDS database.
--
-- ResourceRole - Role (DataPipelineDefaultResourceRole) assumed by an Amazon
-- Elastic Compute Cloud (EC2) instance to carry out the copy task from Amazon
-- RDS to Amazon S3. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates> for data pipelines.
--
-- ServiceRole - Role (DataPipelineDefaultRole) assumed by the AWS Data
-- Pipeline service to monitor the progress of the copy task from Amazon RDS to
-- Amazon Simple Storage Service (S3). For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-iam-roles.html Role templates>
-- for data pipelines.
--
-- SecurityInfo - Security information to use to access an Amazon RDS instance.
-- You need to set up appropriate ingress rules for the security entity IDs
-- provided to allow access to the Amazon RDS instance. Specify a ['SubnetId', 'SecurityGroupIds'] pair for a VPC-based Amazon RDS instance.
--
-- SelectSqlQuery - Query that is used to retrieve the observation data for the 'Datasource'.
--
-- S3StagingLocation - Amazon S3 location for staging RDS data. The data
-- retrieved from Amazon RDS using 'SelectSqlQuery' is stored in this location.
--
-- DataSchemaUri - Amazon S3 location of the 'DataSchema'.
--
-- DataSchema - A JSON string representing the schema. This is not required if 'DataSchemaUri' is specified.
--
-- DataRearrangement - A JSON string representing the splitting requirement of
-- a 'Datasource'.
--
--
-- Sample - ' "{\"randomSeed\":\"some-random-seed\",\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"'
--
--
cdsfrdsRDSData :: Lens' CreateDataSourceFromRDS RDSDataSpec
cdsfrdsRDSData = lens _cdsfrdsRDSData (\s a -> s { _cdsfrdsRDSData = a })

-- | The role that Amazon ML assumes on behalf of the user to create and activate
-- a data pipeline in the user’s account and copy data (using the 'SelectSqlQuery') query from Amazon RDS to Amazon S3.
--
--
--
cdsfrdsRoleARN :: Lens' CreateDataSourceFromRDS Text
cdsfrdsRoleARN = lens _cdsfrdsRoleARN (\s a -> s { _cdsfrdsRoleARN = a })

newtype CreateDataSourceFromRDSResponse = CreateDataSourceFromRDSResponse
    { _cdsfrdsrDataSourceId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateDataSourceFromRDSResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsfrdsrDataSourceId' @::@ 'Maybe' 'Text'
--
createDataSourceFromRDSResponse :: CreateDataSourceFromRDSResponse
createDataSourceFromRDSResponse = CreateDataSourceFromRDSResponse
    { _cdsfrdsrDataSourceId = Nothing
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value should
-- be identical to the value of the 'DataSourceID' in the request.
cdsfrdsrDataSourceId :: Lens' CreateDataSourceFromRDSResponse (Maybe Text)
cdsfrdsrDataSourceId =
    lens _cdsfrdsrDataSourceId (\s a -> s { _cdsfrdsrDataSourceId = a })

instance ToPath CreateDataSourceFromRDS where
    toPath = const "/"

instance ToQuery CreateDataSourceFromRDS where
    toQuery = const mempty

instance ToHeaders CreateDataSourceFromRDS

instance ToJSON CreateDataSourceFromRDS where
    toJSON CreateDataSourceFromRDS{..} = object
        [ "DataSourceId"      .= _cdsfrdsDataSourceId
        , "DataSourceName"    .= _cdsfrdsDataSourceName
        , "RDSData"           .= _cdsfrdsRDSData
        , "RoleARN"           .= _cdsfrdsRoleARN
        , "ComputeStatistics" .= _cdsfrdsComputeStatistics
        ]

instance AWSRequest CreateDataSourceFromRDS where
    type Sv CreateDataSourceFromRDS = MachineLearning
    type Rs CreateDataSourceFromRDS = CreateDataSourceFromRDSResponse

    request  = post "CreateDataSourceFromRDS"
    response = jsonResponse

instance FromJSON CreateDataSourceFromRDSResponse where
    parseJSON = withObject "CreateDataSourceFromRDSResponse" $ \o -> CreateDataSourceFromRDSResponse
        <$> o .:? "DataSourceId"

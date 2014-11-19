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

-- Module      : Network.AWS.EC2.CreateInstanceExportTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Exports a running or stopped instance to an Amazon S3 bucket. For
-- information about the supported operating systems, image formats, and known
-- limitations for the types of instances you can export, see Exporting EC2
-- Instances in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInstanceExportTask.html>
module Network.AWS.EC2.CreateInstanceExportTask
    (
    -- * Request
      CreateInstanceExportTask
    -- ** Request constructor
    , createInstanceExportTask
    -- ** Request lenses
    , cietDescription
    , cietExportToS3Task
    , cietInstanceId
    , cietTargetEnvironment

    -- * Response
    , CreateInstanceExportTaskResponse
    -- ** Response constructor
    , createInstanceExportTaskResponse
    -- ** Response lenses
    , cietrExportTask
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateInstanceExportTask = CreateInstanceExportTask
    { _cietDescription       :: Maybe Text
    , _cietExportToS3Task    :: Maybe ExportToS3TaskSpecification
    , _cietInstanceId        :: Text
    , _cietTargetEnvironment :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateInstanceExportTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cietDescription' @::@ 'Maybe' 'Text'
--
-- * 'cietExportToS3Task' @::@ 'Maybe' 'ExportToS3TaskSpecification'
--
-- * 'cietInstanceId' @::@ 'Text'
--
-- * 'cietTargetEnvironment' @::@ 'Maybe' 'Text'
--
createInstanceExportTask :: Text -- ^ 'cietInstanceId'
                         -> CreateInstanceExportTask
createInstanceExportTask p1 = CreateInstanceExportTask
    { _cietInstanceId        = p1
    , _cietDescription       = Nothing
    , _cietTargetEnvironment = Nothing
    , _cietExportToS3Task    = Nothing
    }

-- | A description for the conversion task or the resource being exported. The
-- maximum length is 255 bytes.
cietDescription :: Lens' CreateInstanceExportTask (Maybe Text)
cietDescription = lens _cietDescription (\s a -> s { _cietDescription = a })

cietExportToS3Task :: Lens' CreateInstanceExportTask (Maybe ExportToS3TaskSpecification)
cietExportToS3Task =
    lens _cietExportToS3Task (\s a -> s { _cietExportToS3Task = a })

-- | The ID of the instance.
cietInstanceId :: Lens' CreateInstanceExportTask Text
cietInstanceId = lens _cietInstanceId (\s a -> s { _cietInstanceId = a })

-- | The target virtualization environment.
cietTargetEnvironment :: Lens' CreateInstanceExportTask (Maybe Text)
cietTargetEnvironment =
    lens _cietTargetEnvironment (\s a -> s { _cietTargetEnvironment = a })

newtype CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { _cietrExportTask :: Maybe ExportTask
    } deriving (Eq, Show)

-- | 'CreateInstanceExportTaskResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cietrExportTask' @::@ 'Maybe' 'ExportTask'
--
createInstanceExportTaskResponse :: CreateInstanceExportTaskResponse
createInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { _cietrExportTask = Nothing
    }

cietrExportTask :: Lens' CreateInstanceExportTaskResponse (Maybe ExportTask)
cietrExportTask = lens _cietrExportTask (\s a -> s { _cietrExportTask = a })

instance ToPath CreateInstanceExportTask where
    toPath = const "/"

instance ToQuery CreateInstanceExportTask where
    toQuery CreateInstanceExportTask{..} = mconcat
        [ "description"       =? _cietDescription
        , "exportToS3"        =? _cietExportToS3Task
        , "instanceId"        =? _cietInstanceId
        , "targetEnvironment" =? _cietTargetEnvironment
        ]

instance ToHeaders CreateInstanceExportTask

instance AWSRequest CreateInstanceExportTask where
    type Sv CreateInstanceExportTask = EC2
    type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse

    request  = post "CreateInstanceExportTask"
    response = xmlResponse

instance FromXML CreateInstanceExportTaskResponse where
    parseXML x = CreateInstanceExportTaskResponse
        <$> x .@? "exportTask"

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example request creates an Export VM task that makes a Windows instance
-- available as an OVA.
-- https://ec2.amazonaws.com/?Action=CreateInstanceExportTask
-- &amp;Description=Example%20for%20docs &amp;InstanceId=i-12345678
-- &amp;TargetEnvironment=VMWare &amp;ExportToS3.DiskImageFormat=VMDK
-- &amp;ExportToS3.ContainerFormat=OVA
-- &amp;ExportToS3.S3bucket=my-bucket-for-exported-vm
-- &amp;ExportToS3.S3prefix=my-exports/ &amp;AUTHPARAMS
-- &lt;CreateInstanceExportTaskResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;exportTask&gt;
-- &lt;exportTaskId&gt;export-i-1234wxyz&lt;/exportTaskId&gt;
-- &lt;description&gt;Example for docs&lt;/description&gt;
-- &lt;state&gt;active&lt;/state&gt;
-- &lt;statusMessage&gt;Running&lt;/statusMessage&gt; &lt;instanceExport&gt;
-- &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;targetEnvironment&gt;VMWare&lt;/targetEnvironment&gt;
-- &lt;/instanceExport&gt; &lt;exportToS3&gt;
-- &lt;diskImageFormat&gt;VMDK&lt;/diskImageFormat&gt;
-- &lt;containerFormat&gt;OVA&lt;/containerFormat&gt;
-- &lt;s3Bucket&gt;my-bucket-for-exported-vm&lt;/s3Bucket&gt;
-- &lt;s3Key&gt;my-exports/ export-i-1234wxyz .ova&lt;/s3Key&gt;
-- &lt;/exportToS3&gt; &lt;/exportTask&gt;
-- &lt;/CreateInstanceExportTaskResponse&gt;.
module Network.AWS.EC2.CreateInstanceExportTask
    (
    -- * Request
      CreateInstanceExportTask
    -- ** Request constructor
    , mkCreateInstanceExportTask
    -- ** Request lenses
    , cietDescription
    , cietInstanceId
    , cietTargetEnvironment
    , cietExportToS3Task

    -- * Response
    , CreateInstanceExportTaskResponse
    -- ** Response constructor
    , mkCreateInstanceExportTaskResponse
    -- ** Response lenses
    , cietrExportTask
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CreateInstanceExportTask = CreateInstanceExportTask
    { _cietDescription :: !(Maybe Text)
    , _cietInstanceId :: !Text
    , _cietTargetEnvironment :: Maybe ExportEnvironment
    , _cietExportToS3Task :: Maybe ExportToS3TaskSpecification
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInstanceExportTask' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Description ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Text@
--
-- * @TargetEnvironment ::@ @Maybe ExportEnvironment@
--
-- * @ExportToS3Task ::@ @Maybe ExportToS3TaskSpecification@
--
mkCreateInstanceExportTask :: Text -- ^ 'cietInstanceId'
                           -> CreateInstanceExportTask
mkCreateInstanceExportTask p2 = CreateInstanceExportTask
    { _cietDescription = Nothing
    , _cietInstanceId = p2
    , _cietTargetEnvironment = Nothing
    , _cietExportToS3Task = Nothing
    }

-- | A description for the conversion task or the resource being exported. The
-- maximum length is 255 bytes.
cietDescription :: Lens' CreateInstanceExportTask (Maybe Text)
cietDescription = lens _cietDescription (\s a -> s { _cietDescription = a })

-- | The ID of the instance.
cietInstanceId :: Lens' CreateInstanceExportTask Text
cietInstanceId = lens _cietInstanceId (\s a -> s { _cietInstanceId = a })

-- | The target virtualization environment.
cietTargetEnvironment :: Lens' CreateInstanceExportTask (Maybe ExportEnvironment)
cietTargetEnvironment =
    lens _cietTargetEnvironment (\s a -> s { _cietTargetEnvironment = a })

-- | 
cietExportToS3Task :: Lens' CreateInstanceExportTask (Maybe ExportToS3TaskSpecification)
cietExportToS3Task =
    lens _cietExportToS3Task (\s a -> s { _cietExportToS3Task = a })

instance ToQuery CreateInstanceExportTask where
    toQuery = genericQuery def

newtype CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { _cietrExportTask :: Maybe ExportTask
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInstanceExportTaskResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ExportTask ::@ @Maybe ExportTask@
--
mkCreateInstanceExportTaskResponse :: CreateInstanceExportTaskResponse
mkCreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { _cietrExportTask = Nothing
    }

-- | 
cietrExportTask :: Lens' CreateInstanceExportTaskResponse (Maybe ExportTask)
cietrExportTask = lens _cietrExportTask (\s a -> s { _cietrExportTask = a })

instance FromXML CreateInstanceExportTaskResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateInstanceExportTask where
    type Sv CreateInstanceExportTask = EC2
    type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse

    request = post "CreateInstanceExportTask"
    response _ = xmlResponse

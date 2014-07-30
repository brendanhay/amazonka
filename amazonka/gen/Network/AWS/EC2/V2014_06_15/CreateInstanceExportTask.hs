{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateInstanceExportTask
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
module Network.AWS.EC2.V2014_06_15.CreateInstanceExportTask where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'CreateInstanceExportTask' request.
createInstanceExportTask :: Text -- ^ '_cietrInstanceId'
                         -> CreateInstanceExportTask
createInstanceExportTask p1 = CreateInstanceExportTask
    { _cietrInstanceId = p1
    , _cietrTargetEnvironment = Nothing
    , _cietrExportToS3Task = Nothing
    , _cietrDescription = Nothing
    }

data CreateInstanceExportTask = CreateInstanceExportTask
    { _cietrInstanceId :: Text
      -- ^ The ID of the instance.
    , _cietrTargetEnvironment :: Maybe ExportEnvironment
      -- ^ The target virtualization environment.
    , _cietrExportToS3Task :: Maybe ExportToS3TaskSpecification
      -- ^ 
    , _cietrDescription :: Maybe Text
      -- ^ A description for the conversion task or the resource being
      -- exported. The maximum length is 255 bytes.
    } deriving (Generic)

instance ToQuery CreateInstanceExportTask where
    toQuery = genericToQuery def

instance AWSRequest CreateInstanceExportTask where
    type Sv CreateInstanceExportTask = EC2
    type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse

    request = post "CreateInstanceExportTask"
    response _ = xmlResponse

data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse
    { _cietsExportTask :: Maybe ExportTask
      -- ^ 
    } deriving (Generic)

instance FromXML CreateInstanceExportTaskResponse where
    fromXMLOptions = xmlOptions

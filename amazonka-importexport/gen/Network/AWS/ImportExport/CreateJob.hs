{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.CreateJob
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation initiates the process of scheduling an upload or download of
-- your data. You include in the request a manifest that describes the data
-- transfer specifics. The response to the request includes a job ID, which
-- you can use in other operations, a signature that you use to identify your
-- storage device, and the address where you should ship your storage device.
module Network.AWS.ImportExport.CreateJob
    (
    -- * Request
      CreateJob
    -- ** Request constructor
    , mkCreateJob
    -- ** Request lenses
    , cj1JobType
    , cj1Manifest
    , cj1ManifestAddendum
    , cj1ValidateOnly

    -- * Response
    , CreateJobResponse
    -- ** Response constructor
    , mkCreateJobResponse
    -- ** Response lenses
    , cjrrJobId
    , cjrrJobType
    , cjrrAwsShippingAddress
    , cjrrSignature
    , cjrrSignatureFileContents
    , cjrrWarningMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types
import Network.AWS.Prelude

-- | Input structure for the CreateJob operation.
data CreateJob = CreateJob
    { _cj1JobType :: JobType
    , _cj1Manifest :: !Text
    , _cj1ManifestAddendum :: !(Maybe Text)
    , _cj1ValidateOnly :: !Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateJob' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @JobType ::@ @JobType@
--
-- * @Manifest ::@ @Text@
--
-- * @ManifestAddendum ::@ @Maybe Text@
--
-- * @ValidateOnly ::@ @Bool@
--
mkCreateJob :: JobType -- ^ 'cj1JobType'
            -> Text -- ^ 'cj1Manifest'
            -> Bool -- ^ 'cj1ValidateOnly'
            -> CreateJob
mkCreateJob p1 p2 p4 = CreateJob
    { _cj1JobType = p1
    , _cj1Manifest = p2
    , _cj1ManifestAddendum = Nothing
    , _cj1ValidateOnly = p4
    }

-- | Specifies whether the job to initiate is an import or export job.
cj1JobType :: Lens' CreateJob JobType
cj1JobType = lens _cj1JobType (\s a -> s { _cj1JobType = a })

-- | The UTF-8 encoded text of the manifest file.
cj1Manifest :: Lens' CreateJob Text
cj1Manifest = lens _cj1Manifest (\s a -> s { _cj1Manifest = a })

-- | For internal use only.
cj1ManifestAddendum :: Lens' CreateJob (Maybe Text)
cj1ManifestAddendum =
    lens _cj1ManifestAddendum (\s a -> s { _cj1ManifestAddendum = a })

-- | Validate the manifest and parameter values in the request but do not
-- actually create a job.
cj1ValidateOnly :: Lens' CreateJob Bool
cj1ValidateOnly = lens _cj1ValidateOnly (\s a -> s { _cj1ValidateOnly = a })

instance ToQuery CreateJob where
    toQuery = genericQuery def

-- | Output structure for the CreateJob operation.
data CreateJobResponse = CreateJobResponse
    { _cjrrJobId :: !(Maybe Text)
    , _cjrrJobType :: Maybe JobType
    , _cjrrAwsShippingAddress :: !(Maybe Text)
    , _cjrrSignature :: !(Maybe Text)
    , _cjrrSignatureFileContents :: !(Maybe Text)
    , _cjrrWarningMessage :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateJobResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @JobId ::@ @Maybe Text@
--
-- * @JobType ::@ @Maybe JobType@
--
-- * @AwsShippingAddress ::@ @Maybe Text@
--
-- * @Signature ::@ @Maybe Text@
--
-- * @SignatureFileContents ::@ @Maybe Text@
--
-- * @WarningMessage ::@ @Maybe Text@
--
mkCreateJobResponse :: CreateJobResponse
mkCreateJobResponse = CreateJobResponse
    { _cjrrJobId = Nothing
    , _cjrrJobType = Nothing
    , _cjrrAwsShippingAddress = Nothing
    , _cjrrSignature = Nothing
    , _cjrrSignatureFileContents = Nothing
    , _cjrrWarningMessage = Nothing
    }

-- | A unique identifier which refers to a particular job.
cjrrJobId :: Lens' CreateJobResponse (Maybe Text)
cjrrJobId = lens _cjrrJobId (\s a -> s { _cjrrJobId = a })

-- | Specifies whether the job to initiate is an import or export job.
cjrrJobType :: Lens' CreateJobResponse (Maybe JobType)
cjrrJobType = lens _cjrrJobType (\s a -> s { _cjrrJobType = a })

-- | Address you ship your storage device to.
cjrrAwsShippingAddress :: Lens' CreateJobResponse (Maybe Text)
cjrrAwsShippingAddress =
    lens _cjrrAwsShippingAddress (\s a -> s { _cjrrAwsShippingAddress = a })

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
cjrrSignature :: Lens' CreateJobResponse (Maybe Text)
cjrrSignature = lens _cjrrSignature (\s a -> s { _cjrrSignature = a })

-- | The actual text of the SIGNATURE file to be written to disk.
cjrrSignatureFileContents :: Lens' CreateJobResponse (Maybe Text)
cjrrSignatureFileContents =
    lens _cjrrSignatureFileContents
         (\s a -> s { _cjrrSignatureFileContents = a })

-- | An optional message notifying you of non-fatal issues with the job, such as
-- use of an incompatible Amazon S3 bucket name.
cjrrWarningMessage :: Lens' CreateJobResponse (Maybe Text)
cjrrWarningMessage =
    lens _cjrrWarningMessage (\s a -> s { _cjrrWarningMessage = a })

instance FromXML CreateJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateJob where
    type Sv CreateJob = ImportExport
    type Rs CreateJob = CreateJobResponse

    request = post "CreateJob"
    response _ = xmlResponse

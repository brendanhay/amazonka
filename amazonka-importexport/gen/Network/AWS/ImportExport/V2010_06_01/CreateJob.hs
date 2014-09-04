{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.CreateJob
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
module Network.AWS.ImportExport.V2010_06_01.CreateJob
    (
    -- * Request
      CreateJob
    -- ** Request constructor
    , mkCreateJobInput
    -- ** Request lenses
    , cjjJobType
    , cjjManifest
    , cjjManifestAddendum
    , cjjValidateOnly

    -- * Response
    , CreateJobResponse
    -- ** Response lenses
    , cjpJobId
    , cjpJobType
    , cjpAwsShippingAddress
    , cjpSignature
    , cjpSignatureFileContents
    , cjpWarningMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateJob' request.
mkCreateJobInput :: JobType -- ^ 'cjjJobType'
                 -> Text -- ^ 'cjjManifest'
                 -> Bool -- ^ 'cjjValidateOnly'
                 -> CreateJob
mkCreateJobInput p1 p2 p3 = CreateJob
    { _cjjJobType = p1
    , _cjjManifest = p2
    , _cjjManifestAddendum = Nothing
    , _cjjValidateOnly = p4
    }
{-# INLINE mkCreateJobInput #-}

data CreateJob = CreateJob
    { _cjjJobType :: JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _cjjManifest :: Text
      -- ^ The UTF-8 encoded text of the manifest file.
    , _cjjManifestAddendum :: Maybe Text
      -- ^ For internal use only.
    , _cjjValidateOnly :: Bool
      -- ^ Validate the manifest and parameter values in the request but do
      -- not actually create a job.
    } deriving (Show, Generic)

-- | Specifies whether the job to initiate is an import or export job.
cjjJobType :: Lens' CreateJob (JobType)
cjjJobType = lens _cjjJobType (\s a -> s { _cjjJobType = a })
{-# INLINE cjjJobType #-}

-- | The UTF-8 encoded text of the manifest file.
cjjManifest :: Lens' CreateJob (Text)
cjjManifest = lens _cjjManifest (\s a -> s { _cjjManifest = a })
{-# INLINE cjjManifest #-}

-- | For internal use only.
cjjManifestAddendum :: Lens' CreateJob (Maybe Text)
cjjManifestAddendum = lens _cjjManifestAddendum (\s a -> s { _cjjManifestAddendum = a })
{-# INLINE cjjManifestAddendum #-}

-- | Validate the manifest and parameter values in the request but do not
-- actually create a job.
cjjValidateOnly :: Lens' CreateJob (Bool)
cjjValidateOnly = lens _cjjValidateOnly (\s a -> s { _cjjValidateOnly = a })
{-# INLINE cjjValidateOnly #-}

instance ToQuery CreateJob where
    toQuery = genericQuery def

data CreateJobResponse = CreateJobResponse
    { _cjpJobId :: Maybe Text
      -- ^ A unique identifier which refers to a particular job.
    , _cjpJobType :: Maybe JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _cjpAwsShippingAddress :: Maybe Text
      -- ^ Address you ship your storage device to.
    , _cjpSignature :: Maybe Text
      -- ^ An encrypted code used to authenticate the request and response,
      -- for example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value
      -- is you want to create the signature file yourself. Generally you
      -- should use the SignatureFileContents value.
    , _cjpSignatureFileContents :: Maybe Text
      -- ^ The actual text of the SIGNATURE file to be written to disk.
    , _cjpWarningMessage :: Maybe Text
      -- ^ An optional message notifying you of non-fatal issues with the
      -- job, such as use of an incompatible Amazon S3 bucket name.
    } deriving (Show, Generic)

-- | A unique identifier which refers to a particular job.
cjpJobId :: Lens' CreateJobResponse (Maybe Text)
cjpJobId = lens _cjpJobId (\s a -> s { _cjpJobId = a })
{-# INLINE cjpJobId #-}

-- | Specifies whether the job to initiate is an import or export job.
cjpJobType :: Lens' CreateJobResponse (Maybe JobType)
cjpJobType = lens _cjpJobType (\s a -> s { _cjpJobType = a })
{-# INLINE cjpJobType #-}

-- | Address you ship your storage device to.
cjpAwsShippingAddress :: Lens' CreateJobResponse (Maybe Text)
cjpAwsShippingAddress = lens _cjpAwsShippingAddress (\s a -> s { _cjpAwsShippingAddress = a })
{-# INLINE cjpAwsShippingAddress #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
cjpSignature :: Lens' CreateJobResponse (Maybe Text)
cjpSignature = lens _cjpSignature (\s a -> s { _cjpSignature = a })
{-# INLINE cjpSignature #-}

-- | The actual text of the SIGNATURE file to be written to disk.
cjpSignatureFileContents :: Lens' CreateJobResponse (Maybe Text)
cjpSignatureFileContents = lens _cjpSignatureFileContents (\s a -> s { _cjpSignatureFileContents = a })
{-# INLINE cjpSignatureFileContents #-}

-- | An optional message notifying you of non-fatal issues with the job, such as
-- use of an incompatible Amazon S3 bucket name.
cjpWarningMessage :: Lens' CreateJobResponse (Maybe Text)
cjpWarningMessage = lens _cjpWarningMessage (\s a -> s { _cjpWarningMessage = a })
{-# INLINE cjpWarningMessage #-}

instance FromXML CreateJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateJob where
    type Sv CreateJob = ImportExport
    type Rs CreateJob = CreateJobResponse

    request = post "CreateJob"
    response _ = xmlResponse

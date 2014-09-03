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
    , createJob
    -- ** Request lenses
    , cjjJobType
    , cjjManifest
    , cjjValidateOnly
    , cjjManifestAddendum

    -- * Response
    , CreateJobResponse
    -- ** Response lenses
    , cjpAwsShippingAddress
    , cjpJobId
    , cjpJobType
    , cjpSignature
    , cjpSignatureFileContents
    , cjpWarningMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateJob' request.
createJob :: JobType -- ^ 'cjjJobType'
          -> Text -- ^ 'cjjManifest'
          -> Bool -- ^ 'cjjValidateOnly'
          -> CreateJob
createJob p1 p2 p3 = CreateJob
    { _cjjJobType = p1
    , _cjjManifest = p2
    , _cjjValidateOnly = p3
    , _cjjManifestAddendum = Nothing
    }

data CreateJob = CreateJob
    { _cjjJobType :: JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
    , _cjjManifest :: Text
      -- ^ The UTF-8 encoded text of the manifest file.
    , _cjjValidateOnly :: Bool
      -- ^ Validate the manifest and parameter values in the request but do
      -- not actually create a job.
    , _cjjManifestAddendum :: Maybe Text
      -- ^ For internal use only.
    } deriving (Show, Generic)

-- | Specifies whether the job to initiate is an import or export job.
cjjJobType
    :: Functor f
    => (JobType
    -> f (JobType))
    -> CreateJob
    -> f CreateJob
cjjJobType f x =
    (\y -> x { _cjjJobType = y })
       <$> f (_cjjJobType x)
{-# INLINE cjjJobType #-}

-- | The UTF-8 encoded text of the manifest file.
cjjManifest
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateJob
    -> f CreateJob
cjjManifest f x =
    (\y -> x { _cjjManifest = y })
       <$> f (_cjjManifest x)
{-# INLINE cjjManifest #-}

-- | Validate the manifest and parameter values in the request but do not
-- actually create a job.
cjjValidateOnly
    :: Functor f
    => (Bool
    -> f (Bool))
    -> CreateJob
    -> f CreateJob
cjjValidateOnly f x =
    (\y -> x { _cjjValidateOnly = y })
       <$> f (_cjjValidateOnly x)
{-# INLINE cjjValidateOnly #-}

-- | For internal use only.
cjjManifestAddendum
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJob
    -> f CreateJob
cjjManifestAddendum f x =
    (\y -> x { _cjjManifestAddendum = y })
       <$> f (_cjjManifestAddendum x)
{-# INLINE cjjManifestAddendum #-}

instance ToQuery CreateJob where
    toQuery = genericQuery def

data CreateJobResponse = CreateJobResponse
    { _cjpAwsShippingAddress :: Maybe Text
      -- ^ Address you ship your storage device to.
    , _cjpJobId :: Maybe Text
      -- ^ A unique identifier which refers to a particular job.
    , _cjpJobType :: Maybe JobType
      -- ^ Specifies whether the job to initiate is an import or export job.
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

-- | Address you ship your storage device to.
cjpAwsShippingAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobResponse
    -> f CreateJobResponse
cjpAwsShippingAddress f x =
    (\y -> x { _cjpAwsShippingAddress = y })
       <$> f (_cjpAwsShippingAddress x)
{-# INLINE cjpAwsShippingAddress #-}

-- | A unique identifier which refers to a particular job.
cjpJobId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobResponse
    -> f CreateJobResponse
cjpJobId f x =
    (\y -> x { _cjpJobId = y })
       <$> f (_cjpJobId x)
{-# INLINE cjpJobId #-}

-- | Specifies whether the job to initiate is an import or export job.
cjpJobType
    :: Functor f
    => (Maybe JobType
    -> f (Maybe JobType))
    -> CreateJobResponse
    -> f CreateJobResponse
cjpJobType f x =
    (\y -> x { _cjpJobType = y })
       <$> f (_cjpJobType x)
{-# INLINE cjpJobType #-}

-- | An encrypted code used to authenticate the request and response, for
-- example, "DV+TpDfx1/TdSE9ktyK9k/bDTVI=". Only use this value is you want to
-- create the signature file yourself. Generally you should use the
-- SignatureFileContents value.
cjpSignature
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobResponse
    -> f CreateJobResponse
cjpSignature f x =
    (\y -> x { _cjpSignature = y })
       <$> f (_cjpSignature x)
{-# INLINE cjpSignature #-}

-- | The actual text of the SIGNATURE file to be written to disk.
cjpSignatureFileContents
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobResponse
    -> f CreateJobResponse
cjpSignatureFileContents f x =
    (\y -> x { _cjpSignatureFileContents = y })
       <$> f (_cjpSignatureFileContents x)
{-# INLINE cjpSignatureFileContents #-}

-- | An optional message notifying you of non-fatal issues with the job, such as
-- use of an incompatible Amazon S3 bucket name.
cjpWarningMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateJobResponse
    -> f CreateJobResponse
cjpWarningMessage f x =
    (\y -> x { _cjpWarningMessage = y })
       <$> f (_cjpWarningMessage x)
{-# INLINE cjpWarningMessage #-}

instance FromXML CreateJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateJob where
    type Sv CreateJob = ImportExport
    type Rs CreateJob = CreateJobResponse

    request = post "CreateJob"
    response _ = xmlResponse

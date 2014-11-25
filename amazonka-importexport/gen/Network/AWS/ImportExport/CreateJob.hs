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
-- transfer specifics. The response to the request includes a job ID, which you
-- can use in other operations, a signature that you use to identify your
-- storage device, and the address where you should ship your storage device.
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebCreateJob.html>
module Network.AWS.ImportExport.CreateJob
    (
    -- * Request
      CreateJob
    -- ** Request constructor
    , createJob
    -- ** Request lenses
    , cjJobType
    , cjManifest
    , cjManifestAddendum
    , cjValidateOnly

    -- * Response
    , CreateJobResponse
    -- ** Response constructor
    , createJobResponse
    -- ** Response lenses
    , cjrAwsShippingAddress
    , cjrJobId
    , cjrJobType
    , cjrSignature
    , cjrSignatureFileContents
    , cjrWarningMessage
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types
import qualified GHC.Exts

data CreateJob = CreateJob
    { _cjJobType          :: JobType
    , _cjManifest         :: Text
    , _cjManifestAddendum :: Maybe Text
    , _cjValidateOnly     :: Bool
    } deriving (Eq, Show)

-- | 'CreateJob' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjJobType' @::@ 'JobType'
--
-- * 'cjManifest' @::@ 'Text'
--
-- * 'cjManifestAddendum' @::@ 'Maybe' 'Text'
--
-- * 'cjValidateOnly' @::@ 'Bool'
--
createJob :: JobType -- ^ 'cjJobType'
          -> Text -- ^ 'cjManifest'
          -> Bool -- ^ 'cjValidateOnly'
          -> CreateJob
createJob p1 p2 p3 = CreateJob
    { _cjJobType          = p1
    , _cjManifest         = p2
    , _cjValidateOnly     = p3
    , _cjManifestAddendum = Nothing
    }

cjJobType :: Lens' CreateJob JobType
cjJobType = lens _cjJobType (\s a -> s { _cjJobType = a })

cjManifest :: Lens' CreateJob Text
cjManifest = lens _cjManifest (\s a -> s { _cjManifest = a })

cjManifestAddendum :: Lens' CreateJob (Maybe Text)
cjManifestAddendum =
    lens _cjManifestAddendum (\s a -> s { _cjManifestAddendum = a })

cjValidateOnly :: Lens' CreateJob Bool
cjValidateOnly = lens _cjValidateOnly (\s a -> s { _cjValidateOnly = a })

data CreateJobResponse = CreateJobResponse
    { _cjrAwsShippingAddress    :: Maybe Text
    , _cjrJobId                 :: Maybe Text
    , _cjrJobType               :: Maybe JobType
    , _cjrSignature             :: Maybe Text
    , _cjrSignatureFileContents :: Maybe Text
    , _cjrWarningMessage        :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateJobResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjrAwsShippingAddress' @::@ 'Maybe' 'Text'
--
-- * 'cjrJobId' @::@ 'Maybe' 'Text'
--
-- * 'cjrJobType' @::@ 'Maybe' 'JobType'
--
-- * 'cjrSignature' @::@ 'Maybe' 'Text'
--
-- * 'cjrSignatureFileContents' @::@ 'Maybe' 'Text'
--
-- * 'cjrWarningMessage' @::@ 'Maybe' 'Text'
--
createJobResponse :: CreateJobResponse
createJobResponse = CreateJobResponse
    { _cjrJobId                 = Nothing
    , _cjrJobType               = Nothing
    , _cjrAwsShippingAddress    = Nothing
    , _cjrSignature             = Nothing
    , _cjrSignatureFileContents = Nothing
    , _cjrWarningMessage        = Nothing
    }

cjrAwsShippingAddress :: Lens' CreateJobResponse (Maybe Text)
cjrAwsShippingAddress =
    lens _cjrAwsShippingAddress (\s a -> s { _cjrAwsShippingAddress = a })

cjrJobId :: Lens' CreateJobResponse (Maybe Text)
cjrJobId = lens _cjrJobId (\s a -> s { _cjrJobId = a })

cjrJobType :: Lens' CreateJobResponse (Maybe JobType)
cjrJobType = lens _cjrJobType (\s a -> s { _cjrJobType = a })

cjrSignature :: Lens' CreateJobResponse (Maybe Text)
cjrSignature = lens _cjrSignature (\s a -> s { _cjrSignature = a })

cjrSignatureFileContents :: Lens' CreateJobResponse (Maybe Text)
cjrSignatureFileContents =
    lens _cjrSignatureFileContents
        (\s a -> s { _cjrSignatureFileContents = a })

cjrWarningMessage :: Lens' CreateJobResponse (Maybe Text)
cjrWarningMessage =
    lens _cjrWarningMessage (\s a -> s { _cjrWarningMessage = a })

instance ToPath CreateJob where
    toPath = const "/"

instance ToQuery CreateJob where
    toQuery CreateJob{..} = mconcat
        [ "JobType"          =? _cjJobType
        , "Manifest"         =? _cjManifest
        , "ManifestAddendum" =? _cjManifestAddendum
        , "ValidateOnly"     =? _cjValidateOnly
        ]

instance ToHeaders CreateJob

instance AWSRequest CreateJob where
    type Sv CreateJob = ImportExport
    type Rs CreateJob = CreateJobResponse

    request  = post "CreateJob"
    response = xmlResponse

instance FromXML CreateJobResponse where
    parseXML = withElement "CreateJobResult" $ \x -> CreateJobResponse
        <$> x .@? "AwsShippingAddress"
        <*> x .@? "JobId"
        <*> x .@? "JobType"
        <*> x .@? "Signature"
        <*> x .@? "SignatureFileContents"
        <*> x .@? "WarningMessage"

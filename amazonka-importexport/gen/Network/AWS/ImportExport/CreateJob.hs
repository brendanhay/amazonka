{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      CreateJobInput
    -- ** Request constructor
    , createJob
    -- ** Request lenses
    , cjiJobType
    , cjiManifest
    , cjiManifestAddendum
    , cjiValidateOnly

    -- * Response
    , CreateJobOutput
    -- ** Response constructor
    , createJobResponse
    -- ** Response lenses
    , cjoAwsShippingAddress
    , cjoJobId
    , cjoJobType
    , cjoSignature
    , cjoSignatureFileContents
    , cjoWarningMessage
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types

data CreateJobInput = CreateJobInput
    { _cjiJobType          :: Text
    , _cjiManifest         :: Text
    , _cjiManifestAddendum :: Maybe Text
    , _cjiValidateOnly     :: Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateJobInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjiJobType' @::@ 'Text'
--
-- * 'cjiManifest' @::@ 'Text'
--
-- * 'cjiManifestAddendum' @::@ 'Maybe' 'Text'
--
-- * 'cjiValidateOnly' @::@ 'Bool'
--
createJob :: Text -- ^ 'cjiJobType'
          -> Text -- ^ 'cjiManifest'
          -> Bool -- ^ 'cjiValidateOnly'
          -> CreateJobInput
createJob p1 p2 p3 = CreateJobInput
    { _cjiJobType          = p1
    , _cjiManifest         = p2
    , _cjiValidateOnly     = p3
    , _cjiManifestAddendum = Nothing
    }

cjiJobType :: Lens' CreateJobInput Text
cjiJobType = lens _cjiJobType (\s a -> s { _cjiJobType = a })

cjiManifest :: Lens' CreateJobInput Text
cjiManifest = lens _cjiManifest (\s a -> s { _cjiManifest = a })

cjiManifestAddendum :: Lens' CreateJobInput (Maybe Text)
cjiManifestAddendum =
    lens _cjiManifestAddendum (\s a -> s { _cjiManifestAddendum = a })

cjiValidateOnly :: Lens' CreateJobInput Bool
cjiValidateOnly = lens _cjiValidateOnly (\s a -> s { _cjiValidateOnly = a })

instance ToPath CreateJobInput where
    toPath = const "/"

instance ToQuery CreateJobInput

data CreateJobOutput = CreateJobOutput
    { _cjoAwsShippingAddress    :: Maybe Text
    , _cjoJobId                 :: Maybe Text
    , _cjoJobType               :: Maybe Text
    , _cjoSignature             :: Maybe Text
    , _cjoSignatureFileContents :: Maybe Text
    , _cjoWarningMessage        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateJobOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjoAwsShippingAddress' @::@ 'Maybe' 'Text'
--
-- * 'cjoJobId' @::@ 'Maybe' 'Text'
--
-- * 'cjoJobType' @::@ 'Maybe' 'Text'
--
-- * 'cjoSignature' @::@ 'Maybe' 'Text'
--
-- * 'cjoSignatureFileContents' @::@ 'Maybe' 'Text'
--
-- * 'cjoWarningMessage' @::@ 'Maybe' 'Text'
--
createJobResponse :: CreateJobOutput
createJobResponse = CreateJobOutput
    { _cjoJobId                 = Nothing
    , _cjoJobType               = Nothing
    , _cjoAwsShippingAddress    = Nothing
    , _cjoSignature             = Nothing
    , _cjoSignatureFileContents = Nothing
    , _cjoWarningMessage        = Nothing
    }

cjoAwsShippingAddress :: Lens' CreateJobOutput (Maybe Text)
cjoAwsShippingAddress =
    lens _cjoAwsShippingAddress (\s a -> s { _cjoAwsShippingAddress = a })

cjoJobId :: Lens' CreateJobOutput (Maybe Text)
cjoJobId = lens _cjoJobId (\s a -> s { _cjoJobId = a })

cjoJobType :: Lens' CreateJobOutput (Maybe Text)
cjoJobType = lens _cjoJobType (\s a -> s { _cjoJobType = a })

cjoSignature :: Lens' CreateJobOutput (Maybe Text)
cjoSignature = lens _cjoSignature (\s a -> s { _cjoSignature = a })

cjoSignatureFileContents :: Lens' CreateJobOutput (Maybe Text)
cjoSignatureFileContents =
    lens _cjoSignatureFileContents
        (\s a -> s { _cjoSignatureFileContents = a })

cjoWarningMessage :: Lens' CreateJobOutput (Maybe Text)
cjoWarningMessage =
    lens _cjoWarningMessage (\s a -> s { _cjoWarningMessage = a })

instance AWSRequest CreateJobInput where
    type Sv CreateJobInput = ImportExport
    type Rs CreateJobInput = CreateJobOutput

    request  = post "CreateJob"
    response = xmlResponse $ \h x -> CreateJobOutput
        <$> x %| "AwsShippingAddress"
        <*> x %| "JobId"
        <*> x %| "JobType"
        <*> x %| "Signature"
        <*> x %| "SignatureFileContents"
        <*> x %| "WarningMessage"

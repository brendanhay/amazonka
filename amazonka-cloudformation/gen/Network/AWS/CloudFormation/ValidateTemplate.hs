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

-- Module      : Network.AWS.CloudFormation.ValidateTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Validates a specified template.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ValidateTemplate.html>
module Network.AWS.CloudFormation.ValidateTemplate
    (
    -- * Request
      ValidateTemplate
    -- ** Request constructor
    , validateTemplate
    -- ** Request lenses
    , vtTemplateBody
    , vtTemplateURL

    -- * Response
    , ValidateTemplateResponse
    -- ** Response constructor
    , validateTemplateResponse
    -- ** Response lenses
    , vtrCapabilities
    , vtrCapabilitiesReason
    , vtrDescription
    , vtrParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data ValidateTemplate = ValidateTemplate
    { _vtTemplateBody :: Maybe Text
    , _vtTemplateURL  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ValidateTemplate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtTemplateBody' @::@ 'Maybe' 'Text'
--
-- * 'vtTemplateURL' @::@ 'Maybe' 'Text'
--
validateTemplate :: ValidateTemplate
validateTemplate = ValidateTemplate
    { _vtTemplateBody = Nothing
    , _vtTemplateURL  = Nothing
    }

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide. Conditional: You
-- must pass TemplateURL or TemplateBody. If both are passed, only
-- TemplateBody is used.
vtTemplateBody :: Lens' ValidateTemplate (Maybe Text)
vtTemplateBody = lens _vtTemplateBody (\s a -> s { _vtTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 307,200 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to Template Anatomy in the
-- AWS CloudFormation User Guide. Conditional: You must pass TemplateURL or
-- TemplateBody. If both are passed, only TemplateBody is used.
vtTemplateURL :: Lens' ValidateTemplate (Maybe Text)
vtTemplateURL = lens _vtTemplateURL (\s a -> s { _vtTemplateURL = a })

data ValidateTemplateResponse = ValidateTemplateResponse
    { _vtrCapabilities       :: List "Capabilities" Text
    , _vtrCapabilitiesReason :: Maybe Text
    , _vtrDescription        :: Maybe Text
    , _vtrParameters         :: List "Parameters" TemplateParameter
    } deriving (Eq, Show)

-- | 'ValidateTemplateResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtrCapabilities' @::@ ['Text']
--
-- * 'vtrCapabilitiesReason' @::@ 'Maybe' 'Text'
--
-- * 'vtrDescription' @::@ 'Maybe' 'Text'
--
-- * 'vtrParameters' @::@ ['TemplateParameter']
--
validateTemplateResponse :: ValidateTemplateResponse
validateTemplateResponse = ValidateTemplateResponse
    { _vtrParameters         = mempty
    , _vtrDescription        = Nothing
    , _vtrCapabilities       = mempty
    , _vtrCapabilitiesReason = Nothing
    }

-- | The capabilities found within the template. Currently, AWS CloudFormation
-- supports only the CAPABILITY_IAM capability. If your template contains
-- IAM resources, you must specify the CAPABILITY_IAM value for this
-- parameter when you use the CreateStack or UpdateStack actions with your
-- template; otherwise, those actions return an InsufficientCapabilities
-- error.
vtrCapabilities :: Lens' ValidateTemplateResponse [Text]
vtrCapabilities = lens _vtrCapabilities (\s a -> s { _vtrCapabilities = a }) . _List

-- | The capabilities reason found within the template.
vtrCapabilitiesReason :: Lens' ValidateTemplateResponse (Maybe Text)
vtrCapabilitiesReason =
    lens _vtrCapabilitiesReason (\s a -> s { _vtrCapabilitiesReason = a })

-- | The description found within the template.
vtrDescription :: Lens' ValidateTemplateResponse (Maybe Text)
vtrDescription = lens _vtrDescription (\s a -> s { _vtrDescription = a })

-- | A list of TemplateParameter structures.
vtrParameters :: Lens' ValidateTemplateResponse [TemplateParameter]
vtrParameters = lens _vtrParameters (\s a -> s { _vtrParameters = a }) . _List

instance ToPath ValidateTemplate where
    toPath = const "/"

instance ToQuery ValidateTemplate where
    toQuery ValidateTemplate{..} = mconcat
        [ "TemplateBody" =? _vtTemplateBody
        , "TemplateURL"  =? _vtTemplateURL
        ]

instance ToHeaders ValidateTemplate

query

instance AWSRequest ValidateTemplate where
    type Sv ValidateTemplate = CloudFormation
    type Rs ValidateTemplate = ValidateTemplateResponse

    request  = post "ValidateTemplate"
    response = xmlResponse

instance FromXML ValidateTemplateResponse where
    parseXML = withElement "ValidateTemplateResult" $ \x -> ValidateTemplateResponse
        <$> x .@  "Capabilities"
        <*> x .@? "CapabilitiesReason"
        <*> x .@? "Description"
        <*> x .@  "Parameters"

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ValidateTemplate
-- &TemplateBody=http://myTemplateRepository/TemplateOne.template
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] false InstanceType Type of instance to launch
-- m1.small false WebServerPort The TCP port for the Web Server 8888 false
-- KeyName Name of an existing EC2 KeyPair to enable SSH access into the
-- server 0be7b6e8-e4a0-11e0-a5bd-9f8d5a7dbc91.
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
    , vtrParameters
    , vtrDescription
    , vtrCapabilities
    , vtrCapabilitiesReason
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude

-- | The input for ValidateTemplate action.
data ValidateTemplate = ValidateTemplate
    { _vtTemplateBody :: Maybe Text
    , _vtTemplateURL :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidateTemplate' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TemplateBody ::@ @Maybe Text@
--
-- * @TemplateURL ::@ @Maybe Text@
--
validateTemplate :: ValidateTemplate
validateTemplate = ValidateTemplate
    { _vtTemplateBody = Nothing
    , _vtTemplateURL = Nothing
    }

-- | Structure containing the template body with a minimum length of 1 byte and
-- a maximum length of 51,200 bytes. For more information, go to Template
-- Anatomy in the AWS CloudFormation User Guide. Conditional: You must pass
-- TemplateURL or TemplateBody. If both are passed, only TemplateBody is used.
vtTemplateBody :: Lens' ValidateTemplate (Maybe Text)
vtTemplateBody = lens _vtTemplateBody (\s a -> s { _vtTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 307,200 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to Template Anatomy in the
-- AWS CloudFormation User Guide. Conditional: You must pass TemplateURL or
-- TemplateBody. If both are passed, only TemplateBody is used.
vtTemplateURL :: Lens' ValidateTemplate (Maybe Text)
vtTemplateURL = lens _vtTemplateURL (\s a -> s { _vtTemplateURL = a })

instance ToQuery ValidateTemplate where
    toQuery = genericQuery def

-- | The output for ValidateTemplate action.
data ValidateTemplateResponse = ValidateTemplateResponse
    { _vtrParameters :: [TemplateParameter]
    , _vtrDescription :: Maybe Text
    , _vtrCapabilities :: [Capability]
    , _vtrCapabilitiesReason :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidateTemplateResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Parameters ::@ @[TemplateParameter]@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Capabilities ::@ @[Capability]@
--
-- * @CapabilitiesReason ::@ @Maybe Text@
--
validateTemplateResponse :: ValidateTemplateResponse
validateTemplateResponse = ValidateTemplateResponse
    { _vtrParameters = mempty
    , _vtrDescription = Nothing
    , _vtrCapabilities = mempty
    , _vtrCapabilitiesReason = Nothing
    }

-- | A list of TemplateParameter structures.
vtrParameters :: Lens' ValidateTemplateResponse [TemplateParameter]
vtrParameters = lens _vtrParameters (\s a -> s { _vtrParameters = a })

-- | The description found within the template.
vtrDescription :: Lens' ValidateTemplateResponse (Maybe Text)
vtrDescription = lens _vtrDescription (\s a -> s { _vtrDescription = a })

-- | The capabilities found within the template. Currently, CAPABILITY_IAM is
-- the only capability detected. If your template contains IAM resources, you
-- must specify the CAPABILITY_IAM value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an InsufficientCapabilities error.
vtrCapabilities :: Lens' ValidateTemplateResponse [Capability]
vtrCapabilities = lens _vtrCapabilities (\s a -> s { _vtrCapabilities = a })

-- | The capabilities reason found within the template.
vtrCapabilitiesReason :: Lens' ValidateTemplateResponse (Maybe Text)
vtrCapabilitiesReason =
    lens _vtrCapabilitiesReason (\s a -> s { _vtrCapabilitiesReason = a })

instance FromXML ValidateTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ValidateTemplate where
    type Sv ValidateTemplate = CloudFormation
    type Rs ValidateTemplate = ValidateTemplateResponse

    request = post "ValidateTemplate"
    response _ = xmlResponse

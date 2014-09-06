{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.ValidateTemplate
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
module Network.AWS.CloudFormation.V2010_05_15.ValidateTemplate
    (
    -- * Request
      ValidateTemplate
    -- ** Request constructor
    , mkValidateTemplate
    -- ** Request lenses
    , vtTemplateBody
    , vtTemplateURL

    -- * Response
    , ValidateTemplateResponse
    -- ** Response lenses
    , vtrsParameters
    , vtrsDescription
    , vtrsCapabilities
    , vtrsCapabilitiesReason
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | The input for ValidateTemplate action.
data ValidateTemplate = ValidateTemplate
    { _vtTemplateBody :: Maybe Text
    , _vtTemplateURL :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidateTemplate' request.
mkValidateTemplate :: ValidateTemplate
mkValidateTemplate = ValidateTemplate
    { _vtTemplateBody = Nothing
    , _vtTemplateURL = Nothing
    }
{-# INLINE mkValidateTemplate #-}

-- | Structure containing the template body with a minimum length of 1 byte and
-- a maximum length of 51,200 bytes. For more information, go to Template
-- Anatomy in the AWS CloudFormation User Guide. Conditional: You must pass
-- TemplateURL or TemplateBody. If both are passed, only TemplateBody is used.
vtTemplateBody :: Lens' ValidateTemplate (Maybe Text)
vtTemplateBody = lens _vtTemplateBody (\s a -> s { _vtTemplateBody = a })
{-# INLINE vtTemplateBody #-}

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 307,200 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to Template Anatomy in the
-- AWS CloudFormation User Guide. Conditional: You must pass TemplateURL or
-- TemplateBody. If both are passed, only TemplateBody is used.
vtTemplateURL :: Lens' ValidateTemplate (Maybe Text)
vtTemplateURL = lens _vtTemplateURL (\s a -> s { _vtTemplateURL = a })
{-# INLINE vtTemplateURL #-}

instance ToQuery ValidateTemplate where
    toQuery = genericQuery def

-- | The output for ValidateTemplate action.
data ValidateTemplateResponse = ValidateTemplateResponse
    { _vtrsParameters :: [TemplateParameter]
    , _vtrsDescription :: Maybe Text
    , _vtrsCapabilities :: [Capability]
    , _vtrsCapabilitiesReason :: Maybe Text
    } deriving (Show, Generic)

-- | A list of TemplateParameter structures.
vtrsParameters :: Lens' ValidateTemplateResponse [TemplateParameter]
vtrsParameters = lens _vtrsParameters (\s a -> s { _vtrsParameters = a })
{-# INLINE vtrsParameters #-}

-- | The description found within the template.
vtrsDescription :: Lens' ValidateTemplateResponse (Maybe Text)
vtrsDescription = lens _vtrsDescription (\s a -> s { _vtrsDescription = a })
{-# INLINE vtrsDescription #-}

-- | The capabilities found within the template. Currently, CAPABILITY_IAM is
-- the only capability detected. If your template contains IAM resources, you
-- must specify the CAPABILITY_IAM value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an InsufficientCapabilities error.
vtrsCapabilities :: Lens' ValidateTemplateResponse [Capability]
vtrsCapabilities =
    lens _vtrsCapabilities (\s a -> s { _vtrsCapabilities = a })
{-# INLINE vtrsCapabilities #-}

-- | The capabilities reason found within the template.
vtrsCapabilitiesReason :: Lens' ValidateTemplateResponse (Maybe Text)
vtrsCapabilitiesReason =
    lens _vtrsCapabilitiesReason (\s a -> s { _vtrsCapabilitiesReason = a })
{-# INLINE vtrsCapabilitiesReason #-}

instance FromXML ValidateTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ValidateTemplate where
    type Sv ValidateTemplate = CloudFormation
    type Rs ValidateTemplate = ValidateTemplateResponse

    request = post "ValidateTemplate"
    response _ = xmlResponse

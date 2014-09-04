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
    , mkValidateTemplateInput
    -- ** Request lenses
    , vtiTemplateBody
    , vtiTemplateURL

    -- * Response
    , ValidateTemplateResponse
    -- ** Response lenses
    , vtoParameters
    , vtoDescription
    , vtoCapabilities
    , vtoCapabilitiesReason
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidateTemplate' request.
mkValidateTemplateInput :: ValidateTemplate
mkValidateTemplateInput = ValidateTemplate
    { _vtiTemplateBody = Nothing
    , _vtiTemplateURL = Nothing
    }
{-# INLINE mkValidateTemplateInput #-}

data ValidateTemplate = ValidateTemplate
    { _vtiTemplateBody :: Maybe Text
      -- ^ Structure containing the template body with a minimum length of 1
      -- byte and a maximum length of 51,200 bytes. For more information,
      -- go to Template Anatomy in the AWS CloudFormation User Guide.
      -- Conditional: You must pass TemplateURL or TemplateBody. If both
      -- are passed, only TemplateBody is used.
    , _vtiTemplateURL :: Maybe Text
      -- ^ Location of file containing the template body. The URL must point
      -- to a template (max size: 307,200 bytes) located in an S3 bucket
      -- in the same region as the stack. For more information, go to
      -- Template Anatomy in the AWS CloudFormation User Guide.
      -- Conditional: You must pass TemplateURL or TemplateBody. If both
      -- are passed, only TemplateBody is used.
    } deriving (Show, Generic)

-- | Structure containing the template body with a minimum length of 1 byte and
-- a maximum length of 51,200 bytes. For more information, go to Template
-- Anatomy in the AWS CloudFormation User Guide. Conditional: You must pass
-- TemplateURL or TemplateBody. If both are passed, only TemplateBody is used.
vtiTemplateBody :: Lens' ValidateTemplate (Maybe Text)
vtiTemplateBody = lens _vtiTemplateBody (\s a -> s { _vtiTemplateBody = a })
{-# INLINE vtiTemplateBody #-}

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 307,200 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to Template Anatomy in the
-- AWS CloudFormation User Guide. Conditional: You must pass TemplateURL or
-- TemplateBody. If both are passed, only TemplateBody is used.
vtiTemplateURL :: Lens' ValidateTemplate (Maybe Text)
vtiTemplateURL = lens _vtiTemplateURL (\s a -> s { _vtiTemplateURL = a })
{-# INLINE vtiTemplateURL #-}

instance ToQuery ValidateTemplate where
    toQuery = genericQuery def

data ValidateTemplateResponse = ValidateTemplateResponse
    { _vtoParameters :: [TemplateParameter]
      -- ^ A list of TemplateParameter structures.
    , _vtoDescription :: Maybe Text
      -- ^ The description found within the template.
    , _vtoCapabilities :: [Capability]
      -- ^ The capabilities found within the template. Currently,
      -- CAPABILITY_IAM is the only capability detected. If your template
      -- contains IAM resources, you must specify the CAPABILITY_IAM value
      -- for this parameter when you use the CreateStack or UpdateStack
      -- actions with your template; otherwise, those actions return an
      -- InsufficientCapabilities error.
    , _vtoCapabilitiesReason :: Maybe Text
      -- ^ The capabilities reason found within the template.
    } deriving (Show, Generic)

-- | A list of TemplateParameter structures.
vtoParameters :: Lens' ValidateTemplateResponse ([TemplateParameter])
vtoParameters = lens _vtoParameters (\s a -> s { _vtoParameters = a })
{-# INLINE vtoParameters #-}

-- | The description found within the template.
vtoDescription :: Lens' ValidateTemplateResponse (Maybe Text)
vtoDescription = lens _vtoDescription (\s a -> s { _vtoDescription = a })
{-# INLINE vtoDescription #-}

-- | The capabilities found within the template. Currently, CAPABILITY_IAM is
-- the only capability detected. If your template contains IAM resources, you
-- must specify the CAPABILITY_IAM value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an InsufficientCapabilities error.
vtoCapabilities :: Lens' ValidateTemplateResponse ([Capability])
vtoCapabilities = lens _vtoCapabilities (\s a -> s { _vtoCapabilities = a })
{-# INLINE vtoCapabilities #-}

-- | The capabilities reason found within the template.
vtoCapabilitiesReason :: Lens' ValidateTemplateResponse (Maybe Text)
vtoCapabilitiesReason = lens _vtoCapabilitiesReason (\s a -> s { _vtoCapabilitiesReason = a })
{-# INLINE vtoCapabilitiesReason #-}

instance FromXML ValidateTemplateResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ValidateTemplate where
    type Sv ValidateTemplate = CloudFormation
    type Rs ValidateTemplate = ValidateTemplateResponse

    request = post "ValidateTemplate"
    response _ = xmlResponse

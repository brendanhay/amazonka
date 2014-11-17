{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.GetTemplateSummary
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about a new or existing template. The
-- GetTemplateSummary action is useful for viewing parameter information, such
-- as default parameter values and parameter types, before you create or
-- update a stack. You can use the GetTemplateSummary action when you submit a
-- template, or you can get template information for a running or deleted
-- stack. For deleted stacks, GetTemplateSummary returns the template
-- information for up to 90 days after the stack has been deleted. If the
-- template does not exist, a ValidationError is returned.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetTemplateSummary.html>
module Network.AWS.CloudFormation.GetTemplateSummary
    (
    -- * Request
      GetTemplateSummary
    -- ** Request constructor
    , getTemplateSummary
    -- ** Request lenses
    , gtsStackName
    , gtsTemplateBody
    , gtsTemplateURL

    -- * Response
    , GetTemplateSummaryResponse
    -- ** Response constructor
    , getTemplateSummaryResponse
    -- ** Response lenses
    , gtsrCapabilities
    , gtsrCapabilitiesReason
    , gtsrDescription
    , gtsrParameters
    , gtsrVersion
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data GetTemplateSummary = GetTemplateSummary
    { _gtsStackName    :: Maybe Text
    , _gtsTemplateBody :: Maybe Text
    , _gtsTemplateURL  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetTemplateSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtsStackName' @::@ 'Maybe' 'Text'
--
-- * 'gtsTemplateBody' @::@ 'Maybe' 'Text'
--
-- * 'gtsTemplateURL' @::@ 'Maybe' 'Text'
--
getTemplateSummary :: GetTemplateSummary
getTemplateSummary = GetTemplateSummary
    { _gtsTemplateBody = Nothing
    , _gtsTemplateURL  = Nothing
    , _gtsStackName    = Nothing
    }

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable. For running stacks, you can specify either
-- the stack's name or its unique stack ID. For deleted stack, you must
-- specify the unique stack ID. Conditional: You must specify only one of
-- the following parameters: StackName, TemplateBody, or TemplateURL.
gtsStackName :: Lens' GetTemplateSummary (Maybe Text)
gtsStackName = lens _gtsStackName (\s a -> s { _gtsStackName = a })

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information about
-- templates, see Template Anatomy in the AWS CloudFormation User Guide.
-- Conditional: You must specify only one of the following parameters:
-- StackName, TemplateBody, or TemplateURL.
gtsTemplateBody :: Lens' GetTemplateSummary (Maybe Text)
gtsTemplateBody = lens _gtsTemplateBody (\s a -> s { _gtsTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 307,200 bytes) located in an Amazon S3 bucket. For
-- more information about templates, see Template Anatomy in the AWS
-- CloudFormation User Guide. Conditional: You must specify only one of the
-- following parameters: StackName, TemplateBody, or TemplateURL.
gtsTemplateURL :: Lens' GetTemplateSummary (Maybe Text)
gtsTemplateURL = lens _gtsTemplateURL (\s a -> s { _gtsTemplateURL = a })

data GetTemplateSummaryResponse = GetTemplateSummaryResponse
    { _gtsrCapabilities       :: [Text]
    , _gtsrCapabilitiesReason :: Maybe Text
    , _gtsrDescription        :: Maybe Text
    , _gtsrParameters         :: [ParameterDeclaration]
    , _gtsrVersion            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetTemplateSummaryResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtsrCapabilities' @::@ ['Text']
--
-- * 'gtsrCapabilitiesReason' @::@ 'Maybe' 'Text'
--
-- * 'gtsrDescription' @::@ 'Maybe' 'Text'
--
-- * 'gtsrParameters' @::@ ['ParameterDeclaration']
--
-- * 'gtsrVersion' @::@ 'Maybe' 'Text'
--
getTemplateSummaryResponse :: GetTemplateSummaryResponse
getTemplateSummaryResponse = GetTemplateSummaryResponse
    { _gtsrParameters         = mempty
    , _gtsrDescription        = Nothing
    , _gtsrCapabilities       = mempty
    , _gtsrCapabilitiesReason = Nothing
    , _gtsrVersion            = Nothing
    }

-- | The capabilities found within the template. Currently, AWS CloudFormation
-- supports only the CAPABILITY_IAM capability. If your template contains
-- IAM resources, you must specify the CAPABILITY_IAM value for this
-- parameter when you use the CreateStack or UpdateStack actions with your
-- template; otherwise, those actions return an InsufficientCapabilities
-- error.
gtsrCapabilities :: Lens' GetTemplateSummaryResponse [Text]
gtsrCapabilities = lens _gtsrCapabilities (\s a -> s { _gtsrCapabilities = a })

-- | The capabilities reason found within the template.
gtsrCapabilitiesReason :: Lens' GetTemplateSummaryResponse (Maybe Text)
gtsrCapabilitiesReason =
    lens _gtsrCapabilitiesReason (\s a -> s { _gtsrCapabilitiesReason = a })

-- | The value that is defined in the Description property of the template.
gtsrDescription :: Lens' GetTemplateSummaryResponse (Maybe Text)
gtsrDescription = lens _gtsrDescription (\s a -> s { _gtsrDescription = a })

-- | A list of parameter declarations that describe various properties for
-- each parameter.
gtsrParameters :: Lens' GetTemplateSummaryResponse [ParameterDeclaration]
gtsrParameters = lens _gtsrParameters (\s a -> s { _gtsrParameters = a })

-- | The AWS template format version, which identifies the capabilities of the
-- template.
gtsrVersion :: Lens' GetTemplateSummaryResponse (Maybe Text)
gtsrVersion = lens _gtsrVersion (\s a -> s { _gtsrVersion = a })

instance AWSRequest GetTemplateSummary where
    type Sv GetTemplateSummary = CloudFormation
    type Rs GetTemplateSummary = GetTemplateSummaryResponse

    request  = post "GetTemplateSummary"
    response = xmlResponse

instance FromXML GetTemplateSummaryResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetTemplateSummaryResponse"

instance ToPath GetTemplateSummary where
    toPath = const "/"

instance ToHeaders GetTemplateSummary

instance ToQuery GetTemplateSummary

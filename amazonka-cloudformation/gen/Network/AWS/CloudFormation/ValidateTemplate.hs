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
module Network.AWS.CloudFormation.ValidateTemplate
    (
    -- * Request
      ValidateTemplateInput
    -- ** Request constructor
    , validateTemplate
    -- ** Request lenses
    , vtiTemplateBody
    , vtiTemplateURL

    -- * Response
    , ValidateTemplateOutput
    -- ** Response constructor
    , validateTemplateResponse
    -- ** Response lenses
    , vtoCapabilities
    , vtoCapabilitiesReason
    , vtoDescription
    , vtoParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data ValidateTemplateInput = ValidateTemplateInput
    { _vtiTemplateBody :: Maybe Text
    , _vtiTemplateURL  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ValidateTemplateInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtiTemplateBody' @::@ 'Maybe' 'Text'
--
-- * 'vtiTemplateURL' @::@ 'Maybe' 'Text'
--
validateTemplate :: ValidateTemplateInput
validateTemplate = ValidateTemplateInput
    { _vtiTemplateBody = Nothing
    , _vtiTemplateURL  = Nothing
    }

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide. Conditional: You
-- must pass TemplateURL or TemplateBody. If both are passed, only
-- TemplateBody is used.
vtiTemplateBody :: Lens' ValidateTemplateInput (Maybe Text)
vtiTemplateBody = lens _vtiTemplateBody (\s a -> s { _vtiTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 307,200 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to Template Anatomy in the
-- AWS CloudFormation User Guide. Conditional: You must pass TemplateURL or
-- TemplateBody. If both are passed, only TemplateBody is used.
vtiTemplateURL :: Lens' ValidateTemplateInput (Maybe Text)
vtiTemplateURL = lens _vtiTemplateURL (\s a -> s { _vtiTemplateURL = a })

instance ToPath ValidateTemplateInput where
    toPath = const "/"

instance ToQuery ValidateTemplateInput

data ValidateTemplateOutput = ValidateTemplateOutput
    { _vtoCapabilities       :: [Text]
    , _vtoCapabilitiesReason :: Maybe Text
    , _vtoDescription        :: Maybe Text
    , _vtoParameters         :: [TemplateParameter]
    } deriving (Eq, Show, Generic)

-- | 'ValidateTemplateOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtoCapabilities' @::@ ['Text']
--
-- * 'vtoCapabilitiesReason' @::@ 'Maybe' 'Text'
--
-- * 'vtoDescription' @::@ 'Maybe' 'Text'
--
-- * 'vtoParameters' @::@ ['TemplateParameter']
--
validateTemplateResponse :: ValidateTemplateOutput
validateTemplateResponse = ValidateTemplateOutput
    { _vtoParameters         = mempty
    , _vtoDescription        = Nothing
    , _vtoCapabilities       = mempty
    , _vtoCapabilitiesReason = Nothing
    }

-- | The capabilities found within the template. Currently, CAPABILITY_IAM is
-- the only capability detected. If your template contains IAM resources,
-- you must specify the CAPABILITY_IAM value for this parameter when you use
-- the CreateStack or UpdateStack actions with your template; otherwise,
-- those actions return an InsufficientCapabilities error.
vtoCapabilities :: Lens' ValidateTemplateOutput [Text]
vtoCapabilities = lens _vtoCapabilities (\s a -> s { _vtoCapabilities = a })

-- | The capabilities reason found within the template.
vtoCapabilitiesReason :: Lens' ValidateTemplateOutput (Maybe Text)
vtoCapabilitiesReason =
    lens _vtoCapabilitiesReason (\s a -> s { _vtoCapabilitiesReason = a })

-- | The description found within the template.
vtoDescription :: Lens' ValidateTemplateOutput (Maybe Text)
vtoDescription = lens _vtoDescription (\s a -> s { _vtoDescription = a })

-- | A list of TemplateParameter structures.
vtoParameters :: Lens' ValidateTemplateOutput [TemplateParameter]
vtoParameters = lens _vtoParameters (\s a -> s { _vtoParameters = a })

instance AWSRequest ValidateTemplateInput where
    type Sv ValidateTemplateInput = CloudFormation
    type Rs ValidateTemplateInput = ValidateTemplateOutput

    request  = post "ValidateTemplate"
    response = xmlResponse $ \h x -> ValidateTemplateOutput
        <$> x %| "Capabilities"
        <*> x %| "CapabilitiesReason"
        <*> x %| "Description"
        <*> x %| "Parameters"

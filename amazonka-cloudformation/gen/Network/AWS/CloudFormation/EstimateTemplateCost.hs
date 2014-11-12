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

-- Module      : Network.AWS.CloudFormation.EstimateTemplateCost
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the estimated monthly cost of a template. The return value is an
-- AWS Simple Monthly Calculator URL with a query string that describes the
-- resources required to run the template.
module Network.AWS.CloudFormation.EstimateTemplateCost
    (
    -- * Request
      EstimateTemplateCostInput
    -- ** Request constructor
    , estimateTemplateCostInput
    -- ** Request lenses
    , etciParameters
    , etciTemplateBody
    , etciTemplateURL

    -- * Response
    , EstimateTemplateCostOutput
    -- ** Response constructor
    , estimateTemplateCostOutput
    -- ** Response lenses
    , etcoUrl
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data EstimateTemplateCostInput = EstimateTemplateCostInput
    { _etciParameters   :: [Parameter]
    , _etciTemplateBody :: Maybe Text
    , _etciTemplateURL  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'EstimateTemplateCostInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etciParameters' @::@ ['Parameter']
--
-- * 'etciTemplateBody' @::@ 'Maybe' 'Text'
--
-- * 'etciTemplateURL' @::@ 'Maybe' 'Text'
--
estimateTemplateCostInput :: EstimateTemplateCostInput
estimateTemplateCostInput = EstimateTemplateCostInput
    { _etciTemplateBody = Nothing
    , _etciTemplateURL  = Nothing
    , _etciParameters   = mempty
    }

-- | A list of Parameter structures that specify input parameters.
etciParameters :: Lens' EstimateTemplateCostInput [Parameter]
etciParameters = lens _etciParameters (\s a -> s { _etciParameters = a })

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide.) Conditional: You
-- must pass TemplateBody or TemplateURL. If both are passed, only
-- TemplateBody is used.
etciTemplateBody :: Lens' EstimateTemplateCostInput (Maybe Text)
etciTemplateBody = lens _etciTemplateBody (\s a -> s { _etciTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For
-- more information, go to Template Anatomy in the AWS CloudFormation User
-- Guide. Conditional: You must pass TemplateURL or TemplateBody. If both
-- are passed, only TemplateBody is used.
etciTemplateURL :: Lens' EstimateTemplateCostInput (Maybe Text)
etciTemplateURL = lens _etciTemplateURL (\s a -> s { _etciTemplateURL = a })

instance ToQuery EstimateTemplateCostInput

instance ToPath EstimateTemplateCostInput where
    toPath = const "/"

newtype EstimateTemplateCostOutput = EstimateTemplateCostOutput
    { _etcoUrl :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'EstimateTemplateCostOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etcoUrl' @::@ 'Maybe' 'Text'
--
estimateTemplateCostOutput :: EstimateTemplateCostOutput
estimateTemplateCostOutput = EstimateTemplateCostOutput
    { _etcoUrl = Nothing
    }

-- | An AWS Simple Monthly Calculator URL with a query string that describes
-- the resources required to run the template.
etcoUrl :: Lens' EstimateTemplateCostOutput (Maybe Text)
etcoUrl = lens _etcoUrl (\s a -> s { _etcoUrl = a })

instance FromXML EstimateTemplateCostOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EstimateTemplateCostOutput"

instance AWSRequest EstimateTemplateCostInput where
    type Sv EstimateTemplateCostInput = CloudFormation
    type Rs EstimateTemplateCostInput = EstimateTemplateCostOutput

    request  = post "EstimateTemplateCost"
    response = xmlResponse $ \h x -> EstimateTemplateCostOutput
        <$> x %| "Url"

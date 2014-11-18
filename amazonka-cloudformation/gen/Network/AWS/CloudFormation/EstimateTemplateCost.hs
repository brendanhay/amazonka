{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_EstimateTemplateCost.html>
module Network.AWS.CloudFormation.EstimateTemplateCost
    (
    -- * Request
      EstimateTemplateCost
    -- ** Request constructor
    , estimateTemplateCost
    -- ** Request lenses
    , etcParameters
    , etcTemplateBody
    , etcTemplateURL

    -- * Response
    , EstimateTemplateCostResponse
    -- ** Response constructor
    , estimateTemplateCostResponse
    -- ** Response lenses
    , etcrUrl
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data EstimateTemplateCost = EstimateTemplateCost
    { _etcParameters   :: [Parameter]
    , _etcTemplateBody :: Maybe Text
    , _etcTemplateURL  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'EstimateTemplateCost' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etcParameters' @::@ ['Parameter']
--
-- * 'etcTemplateBody' @::@ 'Maybe' 'Text'
--
-- * 'etcTemplateURL' @::@ 'Maybe' 'Text'
--
estimateTemplateCost :: EstimateTemplateCost
estimateTemplateCost = EstimateTemplateCost
    { _etcTemplateBody = Nothing
    , _etcTemplateURL  = Nothing
    , _etcParameters   = mempty
    }

-- | A list of Parameter structures that specify input parameters.
etcParameters :: Lens' EstimateTemplateCost [Parameter]
etcParameters = lens _etcParameters (\s a -> s { _etcParameters = a })

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide.) Conditional: You
-- must pass TemplateBody or TemplateURL. If both are passed, only
-- TemplateBody is used.
etcTemplateBody :: Lens' EstimateTemplateCost (Maybe Text)
etcTemplateBody = lens _etcTemplateBody (\s a -> s { _etcTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For
-- more information, go to Template Anatomy in the AWS CloudFormation User
-- Guide. Conditional: You must pass TemplateURL or TemplateBody. If both
-- are passed, only TemplateBody is used.
etcTemplateURL :: Lens' EstimateTemplateCost (Maybe Text)
etcTemplateURL = lens _etcTemplateURL (\s a -> s { _etcTemplateURL = a })

newtype EstimateTemplateCostResponse = EstimateTemplateCostResponse
    { _etcrUrl :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'EstimateTemplateCostResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etcrUrl' @::@ 'Maybe' 'Text'
--
estimateTemplateCostResponse :: EstimateTemplateCostResponse
estimateTemplateCostResponse = EstimateTemplateCostResponse
    { _etcrUrl = Nothing
    }

-- | An AWS Simple Monthly Calculator URL with a query string that describes
-- the resources required to run the template.
etcrUrl :: Lens' EstimateTemplateCostResponse (Maybe Text)
etcrUrl = lens _etcrUrl (\s a -> s { _etcrUrl = a })

instance ToPath EstimateTemplateCost where
    toPath = const "/"

instance ToQuery EstimateTemplateCost

instance ToHeaders EstimateTemplateCost

instance AWSRequest EstimateTemplateCost where
    type Sv EstimateTemplateCost = CloudFormation
    type Rs EstimateTemplateCost = EstimateTemplateCostResponse

    request  = post "EstimateTemplateCost"
    response = xmlResponse

instance FromXML EstimateTemplateCostResponse where
    parseXML c = EstimateTemplateCostResponse
        <$> c .:? "Url"

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.EstimateTemplateCost
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
-- https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=EstimateTemplateCost &TemplateURL=
-- https://s3.amazonaws.com/cloudformation-samples-us-east-1/Drupal_Simple.template
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-12-04T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature]
-- http://calculator.s3.amazonaws.com/calc5.html?key=cf-2e351785-e821-450c-9d58-625e1e1ebfb6.
-- 
module Network.AWS.CloudFormation.V2010_05_15.EstimateTemplateCost where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'EstimateTemplateCost' request.
estimateTemplateCost :: EstimateTemplateCost
estimateTemplateCost = EstimateTemplateCost
    { _etciParameters = mempty
    , _etciTemplateBody = Nothing
    , _etciTemplateURL = Nothing
    }

data EstimateTemplateCost = EstimateTemplateCost
    { _etciParameters :: [Parameter]
      -- ^ A list of Parameter structures that specify input parameters.
    , _etciTemplateBody :: Maybe Text
      -- ^ Structure containing the template body with a minimum length of 1
      -- byte and a maximum length of 51,200 bytes. (For more information,
      -- go to Template Anatomy in the AWS CloudFormation User Guide.)
      -- Conditional: You must pass TemplateBody or TemplateURL. If both
      -- are passed, only TemplateBody is used.
    , _etciTemplateURL :: Maybe Text
      -- ^ Location of file containing the template body. The URL must point
      -- to a template located in an S3 bucket in the same region as the
      -- stack. For more information, go to Template Anatomy in the AWS
      -- CloudFormation User Guide. Conditional: You must pass TemplateURL
      -- or TemplateBody. If both are passed, only TemplateBody is used.
    } deriving (Show, Generic)

makeLenses ''EstimateTemplateCost

instance ToQuery EstimateTemplateCost where
    toQuery = genericToQuery def

data EstimateTemplateCostResponse = EstimateTemplateCostResponse
    { _etcoUrl :: Maybe Text
      -- ^ An AWS Simple Monthly Calculator URL with a query string that
      -- describes the resources required to run the template.
    } deriving (Show, Generic)

makeLenses ''EstimateTemplateCostResponse

instance AWSRequest EstimateTemplateCost where
    type Sv EstimateTemplateCost = CloudFormation
    type Rs EstimateTemplateCost = EstimateTemplateCostResponse

    request = post "EstimateTemplateCost"
    response _ = cursorResponse $ \hs xml ->
        pure EstimateTemplateCostResponse
            <*> xml %|? "Url"

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.Support.V2013_04_15.DescribeServices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current list of AWS services and a list of service categories
-- that applies to each one. You then use service names and categories in your
-- CreateCase requests. Each AWS service has its own set of categories. The
-- service codes and category codes correspond to the values that are
-- displayed in the Service and Category drop-down lists on the AWS Support
-- Center Open a new case page. The values in those fields, however, do not
-- necessarily match the service codes and categories returned by the
-- DescribeServices request. Always use the service codes and categories
-- obtained programmatically. This practice ensures that you always have the
-- most recent set of service and category codes.
module Network.AWS.Support.V2013_04_15.DescribeServices where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Support.V2013_04_15.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'DescribeServices' request.
describeServices :: DescribeServices
describeServices = DescribeServices
    { _dsrLanguage = Nothing
    , _dsrServiceCodeList = mempty
    }

data DescribeServices = DescribeServices
    { _dsrLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides
      -- support. AWS Support currently supports English ("en") and
      -- Japanese ("ja"). Language parameters must be passed explicitly
      -- for operations that take them.
    , _dsrServiceCodeList :: [Text]
      -- ^ A JSON-formatted list of service codes available for AWS
      -- services.
    } deriving (Show, Generic)

makeLenses ''DescribeServices

instance ToPath DescribeServices

instance ToQuery DescribeServices

instance ToHeaders DescribeServices

instance ToJSON DescribeServices

data DescribeServicesResponse = DescribeServicesResponse
    { _dssServices :: [Service]
      -- ^ A JSON-formatted list of AWS services.
    } deriving (Show, Generic)

makeLenses ''DescribeServicesResponse

instance FromJSON DescribeServicesResponse

instance AWSRequest DescribeServices where
    type Sv DescribeServices = Support
    type Rs DescribeServices = DescribeServicesResponse

    request = get
    response _ = jsonResponse

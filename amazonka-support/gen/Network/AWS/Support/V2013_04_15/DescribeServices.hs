{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.Support.V2013_04_15.DescribeServices
    (
    -- * Request
      DescribeServices
    -- ** Request constructor
    , mkDescribeServicesRequest
    -- ** Request lenses
    , dsrServiceCodeList
    , dsrLanguage

    -- * Response
    , DescribeServicesResponse
    -- ** Response lenses
    , dssServices
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServices' request.
mkDescribeServicesRequest :: DescribeServices
mkDescribeServicesRequest = DescribeServices
    { _dsrServiceCodeList = mempty
    , _dsrLanguage = Nothing
    }
{-# INLINE mkDescribeServicesRequest #-}

data DescribeServices = DescribeServices
    { _dsrServiceCodeList :: [Text]
      -- ^ A JSON-formatted list of service codes available for AWS
      -- services.
    , _dsrLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides
      -- support. AWS Support currently supports English ("en") and
      -- Japanese ("ja"). Language parameters must be passed explicitly
      -- for operations that take them.
    } deriving (Show, Generic)

-- | A JSON-formatted list of service codes available for AWS services.
dsrServiceCodeList :: Lens' DescribeServices ([Text])
dsrServiceCodeList = lens _dsrServiceCodeList (\s a -> s { _dsrServiceCodeList = a })
{-# INLINE dsrServiceCodeList #-}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dsrLanguage :: Lens' DescribeServices (Maybe Text)
dsrLanguage = lens _dsrLanguage (\s a -> s { _dsrLanguage = a })
{-# INLINE dsrLanguage #-}

instance ToPath DescribeServices

instance ToQuery DescribeServices

instance ToHeaders DescribeServices

instance ToJSON DescribeServices

newtype DescribeServicesResponse = DescribeServicesResponse
    { _dssServices :: [Service]
      -- ^ A JSON-formatted list of AWS services.
    } deriving (Show, Generic)

-- | A JSON-formatted list of AWS services.
dssServices :: Lens' DescribeServicesResponse ([Service])
dssServices = lens _dssServices (\s a -> s { _dssServices = a })
{-# INLINE dssServices #-}

instance FromJSON DescribeServicesResponse

instance AWSRequest DescribeServices where
    type Sv DescribeServices = Support
    type Rs DescribeServices = DescribeServicesResponse

    request = get
    response _ = jsonResponse

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeServices
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
module Network.AWS.Support.DescribeServices
    (
    -- * Request
      DescribeServices
    -- ** Request constructor
    , describeServices
    -- ** Request lenses
    , dsServiceCodeList
    , dsLanguage

    -- * Response
    , DescribeServicesResponse
    -- ** Response constructor
    , describeServicesResponse
    -- ** Response lenses
    , dsrServices
    ) where

import Network.AWS.Support.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeServices = DescribeServices
    { _dsServiceCodeList :: [Text]
    , _dsLanguage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServices' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ServiceCodeList ::@ @[Text]@
--
-- * @Language ::@ @Maybe Text@
--
describeServices :: DescribeServices
describeServices = DescribeServices
    { _dsServiceCodeList = mempty
    , _dsLanguage = Nothing
    }

-- | A JSON-formatted list of service codes available for AWS services.
dsServiceCodeList :: Lens' DescribeServices [Text]
dsServiceCodeList =
    lens _dsServiceCodeList (\s a -> s { _dsServiceCodeList = a })

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dsLanguage :: Lens' DescribeServices (Maybe Text)
dsLanguage = lens _dsLanguage (\s a -> s { _dsLanguage = a })

instance ToPath DescribeServices

instance ToQuery DescribeServices

instance ToHeaders DescribeServices

instance ToJSON DescribeServices

-- | The list of AWS services returned by the DescribeServices operation.
newtype DescribeServicesResponse = DescribeServicesResponse
    { _dsrServices :: [Service]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServicesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Services ::@ @[Service]@
--
describeServicesResponse :: DescribeServicesResponse
describeServicesResponse = DescribeServicesResponse
    { _dsrServices = mempty
    }

-- | A JSON-formatted list of AWS services.
dsrServices :: Lens' DescribeServicesResponse [Service]
dsrServices = lens _dsrServices (\s a -> s { _dsrServices = a })

instance FromJSON DescribeServicesResponse

instance AWSRequest DescribeServices where
    type Sv DescribeServices = Support
    type Rs DescribeServices = DescribeServicesResponse

    request = get
    response _ = jsonResponse

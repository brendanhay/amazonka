{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the current list of AWS services and a list of service categories
-- that applies to each one. You then use service names and categories in your 'CreateCase' requests. Each AWS service has its own set of categories.
--
-- The service codes and category codes correspond to the values that are
-- displayed in the Service and Category drop-down lists on the AWS Support
-- Center <https://aws.amazon.com/support/createCase Open a new case> page. The values in those fields, however, do not
-- necessarily match the service codes and categories returned by the 'DescribeServices' request. Always use the service codes and categories obtained
-- programmatically. This practice ensures that you always have the most recent
-- set of service and category codes.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeServices.html>
module Network.AWS.Support.DescribeServices
    (
    -- * Request
      DescribeServices
    -- ** Request constructor
    , describeServices
    -- ** Request lenses
    , dsLanguage
    , dsServiceCodeList

    -- * Response
    , DescribeServicesResponse
    -- ** Response constructor
    , describeServicesResponse
    -- ** Response lenses
    , dsrServices
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Support.Types
import qualified GHC.Exts

data DescribeServices = DescribeServices
    { _dsLanguage        :: Maybe Text
    , _dsServiceCodeList :: List "serviceCodeList" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeServices' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsLanguage' @::@ 'Maybe' 'Text'
--
-- * 'dsServiceCodeList' @::@ ['Text']
--
describeServices :: DescribeServices
describeServices = DescribeServices
    { _dsServiceCodeList = mempty
    , _dsLanguage        = Nothing
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dsLanguage :: Lens' DescribeServices (Maybe Text)
dsLanguage = lens _dsLanguage (\s a -> s { _dsLanguage = a })

-- | A JSON-formatted list of service codes available for AWS services.
dsServiceCodeList :: Lens' DescribeServices [Text]
dsServiceCodeList =
    lens _dsServiceCodeList (\s a -> s { _dsServiceCodeList = a })
        . _List

newtype DescribeServicesResponse = DescribeServicesResponse
    { _dsrServices :: List "services" SupportService
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeServicesResponse where
    type Item DescribeServicesResponse = SupportService

    fromList = DescribeServicesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsrServices

-- | 'DescribeServicesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrServices' @::@ ['SupportService']
--
describeServicesResponse :: DescribeServicesResponse
describeServicesResponse = DescribeServicesResponse
    { _dsrServices = mempty
    }

-- | A JSON-formatted list of AWS services.
dsrServices :: Lens' DescribeServicesResponse [SupportService]
dsrServices = lens _dsrServices (\s a -> s { _dsrServices = a }) . _List

instance ToPath DescribeServices where
    toPath = const "/"

instance ToQuery DescribeServices where
    toQuery = const mempty

instance ToHeaders DescribeServices

instance ToJSON DescribeServices where
    toJSON DescribeServices{..} = object
        [ "serviceCodeList" .= _dsServiceCodeList
        , "language"        .= _dsLanguage
        ]

instance AWSRequest DescribeServices where
    type Sv DescribeServices = Support
    type Rs DescribeServices = DescribeServicesResponse

    request  = post "DescribeServices"
    response = jsonResponse

instance FromJSON DescribeServicesResponse where
    parseJSON = withObject "DescribeServicesResponse" $ \o -> DescribeServicesResponse
        <$> o .:  "services"

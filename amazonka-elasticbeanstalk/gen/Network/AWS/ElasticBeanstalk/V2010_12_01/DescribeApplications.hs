{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the descriptions of existing applications.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationNames.member.1=SampleApplication
-- &Operation=DescribeApplications &AuthParams Sample Description
-- SampleApplication 2010-11-16T20:20:51.974Z 2010-11-16T20:20:51.974Z Default
-- 577c70ff-f1d7-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeApplications
    (
    -- * Request
      DescribeApplications
    -- ** Request constructor
    , describeApplications
    -- ** Request lenses
    , danApplicationNames

    -- * Response
    , DescribeApplicationsResponse
    -- ** Response lenses
    , adnApplications
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeApplications' request.
describeApplications :: DescribeApplications
describeApplications = DescribeApplications
    { _danApplicationNames = mempty
    }
{-# INLINE describeApplications #-}

data DescribeApplications = DescribeApplications
    { _danApplicationNames :: [Text]
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to only include those with the specified names.
    } deriving (Show, Generic)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- only include those with the specified names.
danApplicationNames :: Lens' DescribeApplications ([Text])
danApplicationNames f x =
    f (_danApplicationNames x)
        <&> \y -> x { _danApplicationNames = y }
{-# INLINE danApplicationNames #-}

instance ToQuery DescribeApplications where
    toQuery = genericQuery def

data DescribeApplicationsResponse = DescribeApplicationsResponse
    { _adnApplications :: [ApplicationDescription]
      -- ^ This parameter contains a list of ApplicationDescription.
    } deriving (Show, Generic)

-- | This parameter contains a list of ApplicationDescription.
adnApplications :: Lens' DescribeApplicationsResponse ([ApplicationDescription])
adnApplications f x =
    f (_adnApplications x)
        <&> \y -> x { _adnApplications = y }
{-# INLINE adnApplications #-}

instance FromXML DescribeApplicationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeApplications where
    type Sv DescribeApplications = ElasticBeanstalk
    type Rs DescribeApplications = DescribeApplicationsResponse

    request = post "DescribeApplications"
    response _ = xmlResponse

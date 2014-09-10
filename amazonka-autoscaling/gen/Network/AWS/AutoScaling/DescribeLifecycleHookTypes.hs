{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the available types of lifecycle hooks.
module Network.AWS.AutoScaling
    (
    -- * Request
      DescribeLifecycleHookTypes
    -- ** Request constructor
    , mkDescribeLifecycleHookTypes
    -- * Response
    , DescribeLifecycleHookTypesResponse
    -- ** Response constructor
    , mkDescribeLifecycleHookTypesResponse
    -- ** Response lenses
    , dlhtrLifecycleHookTypes
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLifecycleHookTypes' request.
mkDescribeLifecycleHookTypes :: DescribeLifecycleHookTypes
mkDescribeLifecycleHookTypes = DescribeLifecycleHookTypes

instance ToQuery DescribeLifecycleHookTypes where
    toQuery = genericQuery def

newtype DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse
    { _dlhtrLifecycleHookTypes :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLifecycleHookTypesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LifecycleHookTypes ::@ @[Text]@
--
mkDescribeLifecycleHookTypesResponse :: DescribeLifecycleHookTypesResponse
mkDescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse
    { _dlhtrLifecycleHookTypes = mempty
    }

-- | Returns a list of all notification types supported by Auto Scaling. They
-- are: autoscaling:EC2_INSTANCE_LAUNCHING
-- autoscaling:EC2_INSTANCE_TERMINATING.
dlhtrLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesResponse [Text]
dlhtrLifecycleHookTypes =
    lens _dlhtrLifecycleHookTypes
         (\s a -> s { _dlhtrLifecycleHookTypes = a })

instance FromXML DescribeLifecycleHookTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLifecycleHookTypes where
    type Sv DescribeLifecycleHookTypes = AutoScaling
    type Rs DescribeLifecycleHookTypes = DescribeLifecycleHookTypesResponse

    request = post "DescribeLifecycleHookTypes"
    response _ = xmlResponse

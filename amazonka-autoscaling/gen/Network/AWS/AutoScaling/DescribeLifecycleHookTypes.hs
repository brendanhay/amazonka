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

-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
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

-- | Describes the available types of lifecycle hooks.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLifecycleHookTypes.html>
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
    (
    -- * Request
      DescribeLifecycleHookTypes
    -- ** Request constructor
    , describeLifecycleHookTypes

    -- * Response
    , DescribeLifecycleHookTypesResponse
    -- ** Response constructor
    , describeLifecycleHookTypesResponse
    -- ** Response lenses
    , dlhtrLifecycleHookTypes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeLifecycleHookTypes' constructor.
describeLifecycleHookTypes :: DescribeLifecycleHookTypes
describeLifecycleHookTypes = DescribeLifecycleHookTypes

newtype DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse
    { _dlhtrLifecycleHookTypes :: List "LifecycleHookTypes" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeLifecycleHookTypesResponse where
    type Item DescribeLifecycleHookTypesResponse = Text

    fromList = DescribeLifecycleHookTypesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dlhtrLifecycleHookTypes

-- | 'DescribeLifecycleHookTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhtrLifecycleHookTypes' @::@ ['Text']
--
describeLifecycleHookTypesResponse :: DescribeLifecycleHookTypesResponse
describeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse
    { _dlhtrLifecycleHookTypes = mempty
    }

-- | One or more of the following notification types:
--
-- 'autoscaling:EC2_INSTANCE_LAUNCHING'
--
-- 'autoscaling:EC2_INSTANCE_TERMINATING'
--
--
dlhtrLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesResponse [Text]
dlhtrLifecycleHookTypes =
    lens _dlhtrLifecycleHookTypes (\s a -> s { _dlhtrLifecycleHookTypes = a })
        . _List

instance ToPath DescribeLifecycleHookTypes where
    toPath = const "/"

instance ToQuery DescribeLifecycleHookTypes where
    toQuery = const mempty

instance ToHeaders DescribeLifecycleHookTypes

instance AWSRequest DescribeLifecycleHookTypes where
    type Sv DescribeLifecycleHookTypes = AutoScaling
    type Rs DescribeLifecycleHookTypes = DescribeLifecycleHookTypesResponse

    request  = post "DescribeLifecycleHookTypes"
    response = xmlResponse

instance FromXML DescribeLifecycleHookTypesResponse where
    parseXML = withElement "DescribeLifecycleHookTypesResult" $ \x -> DescribeLifecycleHookTypesResponse
        <$> x .@  "LifecycleHookTypes"

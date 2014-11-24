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

-- Module      : Network.AWS.SWF.DescribeActivityType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified activity type. This includes
-- configuration settings provided at registration time as well as other
-- general information about the type. Access Control You can use IAM policies
-- to control this action's access to Amazon SWF resources as follows: Use a
-- @Resource@ element with the domain name to limit the action to only
-- specified domains. Use an @Action@ element to allow or deny permission to
-- call this action. Constrain the following parameters by using a @Condition@
-- element with the appropriate keys. @activityType.name@: String constraint.
-- The key is @swf:activityType.name@. @activityType.version@: String
-- constraint. The key is @swf:activityType.version@. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- @OperationNotPermitted@. For details and example IAM policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html
-- Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DescribeActivityType.html>
module Network.AWS.SWF.DescribeActivityType
    (
    -- * Request
      DescribeActivityType
    -- ** Request constructor
    , describeActivityType
    -- ** Request lenses
    , datActivityType
    , datDomain

    -- * Response
    , DescribeActivityTypeResponse
    -- ** Response constructor
    , describeActivityTypeResponse
    -- ** Response lenses
    , datrConfiguration
    , datrTypeInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data DescribeActivityType = DescribeActivityType
    { _datActivityType :: ActivityType
    , _datDomain       :: Text
    } deriving (Eq, Show)

-- | 'DescribeActivityType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datActivityType' @::@ 'ActivityType'
--
-- * 'datDomain' @::@ 'Text'
--
describeActivityType :: Text -- ^ 'datDomain'
                     -> ActivityType -- ^ 'datActivityType'
                     -> DescribeActivityType
describeActivityType p1 p2 = DescribeActivityType
    { _datDomain       = p1
    , _datActivityType = p2
    }

-- | The activity type to describe.
datActivityType :: Lens' DescribeActivityType ActivityType
datActivityType = lens _datActivityType (\s a -> s { _datActivityType = a })

-- | The name of the domain in which the activity type is registered.
datDomain :: Lens' DescribeActivityType Text
datDomain = lens _datDomain (\s a -> s { _datDomain = a })

data DescribeActivityTypeResponse = DescribeActivityTypeResponse
    { _datrConfiguration :: ActivityTypeConfiguration
    , _datrTypeInfo      :: ActivityTypeInfo
    } deriving (Eq, Show)

-- | 'DescribeActivityTypeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datrConfiguration' @::@ 'ActivityTypeConfiguration'
--
-- * 'datrTypeInfo' @::@ 'ActivityTypeInfo'
--
describeActivityTypeResponse :: ActivityTypeInfo -- ^ 'datrTypeInfo'
                             -> ActivityTypeConfiguration -- ^ 'datrConfiguration'
                             -> DescribeActivityTypeResponse
describeActivityTypeResponse p1 p2 = DescribeActivityTypeResponse
    { _datrTypeInfo      = p1
    , _datrConfiguration = p2
    }

-- | The configuration settings registered with the activity type.
datrConfiguration :: Lens' DescribeActivityTypeResponse ActivityTypeConfiguration
datrConfiguration =
    lens _datrConfiguration (\s a -> s { _datrConfiguration = a })

-- | General information about the activity type. The status of activity type
-- (returned in the ActivityTypeInfo structure) can be one of the following.
-- REGISTERED: The type is registered and available. Workers supporting this
-- type should be running. DEPRECATED: The type was deprecated using
-- 'DeprecateActivityType', but is still in use. You should keep workers
-- supporting this type running. You cannot create new tasks of this type.
datrTypeInfo :: Lens' DescribeActivityTypeResponse ActivityTypeInfo
datrTypeInfo = lens _datrTypeInfo (\s a -> s { _datrTypeInfo = a })

instance ToPath DescribeActivityType where
    toPath = const "/"

instance ToQuery DescribeActivityType where
    toQuery = const mempty

instance ToHeaders DescribeActivityType

instance ToJSON DescribeActivityType where
    toJSON DescribeActivityType{..} = object
        [ "domain"       .= _datDomain
        , "activityType" .= _datActivityType
        ]

instance AWSRequest DescribeActivityType where
    type Sv DescribeActivityType = SWF
    type Rs DescribeActivityType = DescribeActivityTypeResponse

    request  = post "DescribeActivityType"
    response = jsonResponse

instance FromJSON DescribeActivityTypeResponse where
    parseJSON = withObject "DescribeActivityTypeResponse" $ \o -> DescribeActivityTypeResponse
        <$> o .:  "configuration"
        <*> o .:  "typeInfo"

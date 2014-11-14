{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.SWF.DeprecateActivityType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deprecates the specified activity type. After an activity type has been
-- deprecated, you cannot create new tasks of that activity type. Tasks of
-- this type that were scheduled before the type was deprecated will continue
-- to run. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. Constrain the
-- following parameters by using a Condition element with the appropriate
-- keys. activityType.name: String constraint. The key is
-- swf:activityType.name. activityType.version: String constraint. The key is
-- swf:activityType.version. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows.
module Network.AWS.SWF.DeprecateActivityType
    (
    -- * Request
      DeprecateActivityType
    -- ** Request constructor
    , deprecateActivityType
    -- ** Request lenses
    , dat1ActivityType
    , dat1Domain

    -- * Response
    , DeprecateActivityTypeResponse
    -- ** Response constructor
    , deprecateActivityTypeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.SWF.Types

data DeprecateActivityType = DeprecateActivityType
    { _dat1ActivityType :: ActivityType
    , _dat1Domain       :: Text
    } deriving (Eq, Show, Generic)

-- | 'DeprecateActivityType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dat1ActivityType' @::@ 'ActivityType'
--
-- * 'dat1Domain' @::@ 'Text'
--
deprecateActivityType :: Text -- ^ 'dat1Domain'
                      -> ActivityType -- ^ 'dat1ActivityType'
                      -> DeprecateActivityType
deprecateActivityType p1 p2 = DeprecateActivityType
    { _dat1Domain       = p1
    , _dat1ActivityType = p2
    }

-- | The activity type to deprecate.
dat1ActivityType :: Lens' DeprecateActivityType ActivityType
dat1ActivityType = lens _dat1ActivityType (\s a -> s { _dat1ActivityType = a })

-- | The name of the domain in which the activity type is registered.
dat1Domain :: Lens' DeprecateActivityType Text
dat1Domain = lens _dat1Domain (\s a -> s { _dat1Domain = a })

instance ToPath DeprecateActivityType where
    toPath = const "/"

instance ToQuery DeprecateActivityType where
    toQuery = const mempty

instance ToHeaders DeprecateActivityType

instance ToBody DeprecateActivityType where
    toBody = toBody . encode . _dat1Domain

data DeprecateActivityTypeResponse = DeprecateActivityTypeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeprecateActivityTypeResponse' constructor.
deprecateActivityTypeResponse :: DeprecateActivityTypeResponse
deprecateActivityTypeResponse = DeprecateActivityTypeResponse

instance AWSRequest DeprecateActivityType where
    type Sv DeprecateActivityType = SWF
    type Rs DeprecateActivityType = DeprecateActivityTypeResponse

    request  = post
    response = nullaryResponse DeprecateActivityTypeResponse

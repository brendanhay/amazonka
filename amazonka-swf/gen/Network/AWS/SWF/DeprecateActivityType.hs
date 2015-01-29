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

-- Module      : Network.AWS.SWF.DeprecateActivityType
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

-- | Deprecates the specified /activity type/. After an activity type has been
-- deprecated, you cannot create new tasks of that activity type. Tasks of this
-- type that were scheduled before the type was deprecated will continue to run.
--
-- This operation is eventually consistent. The results are best effort and may
-- not exactly reflect recent updates and changes. Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. Constrain the following parameters by using a 'Condition' element
-- with the appropriate keys.  'activityType.name': String constraint. The key is 'swf:activityType.name'. 'activityType.version': String constraint. The key is 'swf:activityType.version'
-- .    If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the action
-- fails. The associated event attribute's cause parameter will be set to
-- OPERATION_NOT_PERMITTED. For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAMto Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DeprecateActivityType.html>
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
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data DeprecateActivityType = DeprecateActivityType
    { _dat1ActivityType :: ActivityType
    , _dat1Domain       :: Text
    } deriving (Eq, Read, Show)

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

data DeprecateActivityTypeResponse = DeprecateActivityTypeResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeprecateActivityTypeResponse' constructor.
deprecateActivityTypeResponse :: DeprecateActivityTypeResponse
deprecateActivityTypeResponse = DeprecateActivityTypeResponse

instance ToPath DeprecateActivityType where
    toPath = const "/"

instance ToQuery DeprecateActivityType where
    toQuery = const mempty

instance ToHeaders DeprecateActivityType

instance ToJSON DeprecateActivityType where
    toJSON DeprecateActivityType{..} = object
        [ "domain"       .= _dat1Domain
        , "activityType" .= _dat1ActivityType
        ]

instance AWSRequest DeprecateActivityType where
    type Sv DeprecateActivityType = SWF
    type Rs DeprecateActivityType = DeprecateActivityTypeResponse

    request  = post "DeprecateActivityType"
    response = nullResponse DeprecateActivityTypeResponse

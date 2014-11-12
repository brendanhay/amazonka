{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions of what each policy does. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
module Network.AWS.AutoScaling.DescribePolicies
    (
    -- * Request
      DescribePoliciesType
    -- ** Request constructor
    , describePoliciesType
    -- ** Request lenses
    , dptAutoScalingGroupName
    , dptMaxRecords
    , dptNextToken
    , dptPolicyNames

    -- * Response
    , PoliciesType
    -- ** Response constructor
    , policiesType
    -- ** Response lenses
    , ptNextToken
    , ptScalingPolicies
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribePoliciesType = DescribePoliciesType
    { _dptAutoScalingGroupName :: Maybe Text
    , _dptMaxRecords           :: Maybe Int
    , _dptNextToken            :: Maybe Text
    , _dptPolicyNames          :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribePoliciesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dptAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dptMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dptNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dptPolicyNames' @::@ ['Text']
--
describePoliciesType :: DescribePoliciesType
describePoliciesType = DescribePoliciesType
    { _dptAutoScalingGroupName = Nothing
    , _dptPolicyNames          = mempty
    , _dptNextToken            = Nothing
    , _dptMaxRecords           = Nothing
    }

-- | The name of the Auto Scaling group.
dptAutoScalingGroupName :: Lens' DescribePoliciesType (Maybe Text)
dptAutoScalingGroupName =
    lens _dptAutoScalingGroupName (\s a -> s { _dptAutoScalingGroupName = a })

-- | The maximum number of policies that will be described with each call.
dptMaxRecords :: Lens' DescribePoliciesType (Maybe Int)
dptMaxRecords = lens _dptMaxRecords (\s a -> s { _dptMaxRecords = a })

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dptNextToken :: Lens' DescribePoliciesType (Maybe Text)
dptNextToken = lens _dptNextToken (\s a -> s { _dptNextToken = a })

-- | A list of policy names or policy ARNs to be described. If this list is
-- omitted, all policy names are described. If an auto scaling group name is
-- provided, the results are limited to that group. The list of requested
-- policy names cannot contain more than 50 items. If unknown policy names
-- are requested, they are ignored with no error.
dptPolicyNames :: Lens' DescribePoliciesType [Text]
dptPolicyNames = lens _dptPolicyNames (\s a -> s { _dptPolicyNames = a })

instance ToQuery DescribePoliciesType

instance ToPath DescribePoliciesType where
    toPath = const "/"

data PoliciesType = PoliciesType
    { _ptNextToken       :: Maybe Text
    , _ptScalingPolicies :: [ScalingPolicy]
    } deriving (Eq, Show, Generic)

-- | 'PoliciesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptNextToken' @::@ 'Maybe' 'Text'
--
-- * 'ptScalingPolicies' @::@ ['ScalingPolicy']
--
policiesType :: PoliciesType
policiesType = PoliciesType
    { _ptScalingPolicies = mempty
    , _ptNextToken       = Nothing
    }

-- | A string that marks the start of the next batch of returned results.
ptNextToken :: Lens' PoliciesType (Maybe Text)
ptNextToken = lens _ptNextToken (\s a -> s { _ptNextToken = a })

-- | A list of scaling policies.
ptScalingPolicies :: Lens' PoliciesType [ScalingPolicy]
ptScalingPolicies =
    lens _ptScalingPolicies (\s a -> s { _ptScalingPolicies = a })

instance FromXML PoliciesType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PoliciesType"

instance AWSRequest DescribePoliciesType where
    type Sv DescribePoliciesType = AutoScaling
    type Rs DescribePoliciesType = PoliciesType

    request  = post "DescribePolicies"
    response = xmlResponse $ \h x -> PoliciesType
        <$> x %| "NextToken"
        <*> x %| "ScalingPolicies"

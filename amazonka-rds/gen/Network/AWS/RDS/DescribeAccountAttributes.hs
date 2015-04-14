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

-- Module      : Network.AWS.RDS.DescribeAccountAttributes
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

-- | Lists all of the attributes for a customer account. The attributes include
-- Amazon RDS quotas for the account, such as the number of DB instances
-- allowed. The description for a quota includes the quota name, current usage
-- toward that quota, and the quota's maximum value.
--
-- This command does not take any parameters.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeAccountAttributes.html>
module Network.AWS.RDS.DescribeAccountAttributes
    (
    -- * Request
      DescribeAccountAttributes
    -- ** Request constructor
    , describeAccountAttributes

    -- * Response
    , DescribeAccountAttributesResponse
    -- ** Response constructor
    , describeAccountAttributesResponse
    -- ** Response lenses
    , daarAccountQuotas
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeAccountAttributes = DescribeAccountAttributes
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DescribeAccountAttributes' constructor.
describeAccountAttributes :: DescribeAccountAttributes
describeAccountAttributes = DescribeAccountAttributes

newtype DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { _daarAccountQuotas :: List "member" AccountQuota
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeAccountAttributesResponse where
    type Item DescribeAccountAttributesResponse = AccountQuota

    fromList = DescribeAccountAttributesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _daarAccountQuotas

-- | 'DescribeAccountAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daarAccountQuotas' @::@ ['AccountQuota']
--
describeAccountAttributesResponse :: DescribeAccountAttributesResponse
describeAccountAttributesResponse = DescribeAccountAttributesResponse
    { _daarAccountQuotas = mempty
    }

-- | A list of 'AccountQuota' objects. Within this list, each quota has a name, a
-- count of usage toward the quota maximum, and a maximum value for the quota.
daarAccountQuotas :: Lens' DescribeAccountAttributesResponse [AccountQuota]
daarAccountQuotas =
    lens _daarAccountQuotas (\s a -> s { _daarAccountQuotas = a })
        . _List

instance ToPath DescribeAccountAttributes where
    toPath = const "/"

instance ToQuery DescribeAccountAttributes where
    toQuery = const mempty

instance ToHeaders DescribeAccountAttributes

instance AWSRequest DescribeAccountAttributes where
    type Sv DescribeAccountAttributes = RDS
    type Rs DescribeAccountAttributes = DescribeAccountAttributesResponse

    request  = post "DescribeAccountAttributes"
    response = xmlResponse

instance FromXML DescribeAccountAttributesResponse where
    parseXML = withElement "DescribeAccountAttributesResult" $ \x -> DescribeAccountAttributesResponse
        <$> x .@? "AccountQuotas" .!@ mempty

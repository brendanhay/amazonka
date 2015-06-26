{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists all of the attributes for a customer account. The attributes
-- include Amazon RDS quotas for the account, such as the number of DB
-- instances allowed. The description for a quota includes the quota name,
-- current usage toward that quota, and the quota\'s maximum value.
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
    , daarStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
-- /See:/ 'describeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes' deriving (Eq, Read, Show)

-- | 'DescribeAccountAttributes' smart constructor.
describeAccountAttributes :: DescribeAccountAttributes
describeAccountAttributes = DescribeAccountAttributes';

instance AWSRequest DescribeAccountAttributes where
        type Sv DescribeAccountAttributes = RDS
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAccountAttributesResult"
              (\ s h x ->
                 DescribeAccountAttributesResponse' <$>
                   (x .@? "AccountQuotas" .!@ mempty >>=
                      may (parseXMLList "AccountQuota"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeAccountAttributes where
        toHeaders = const mempty

instance ToPath DescribeAccountAttributes where
        toPath = const "/"

instance ToQuery DescribeAccountAttributes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeAccountAttributes" :: ByteString),
                  "Version" =: ("2014-10-31" :: ByteString)])

-- | Data returned by the __DescribeAccountAttributes__ action.
--
-- /See:/ 'describeAccountAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daarAccountQuotas'
--
-- * 'daarStatusCode'
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'{_daarAccountQuotas :: Maybe [AccountQuota], _daarStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeAccountAttributesResponse' smart constructor.
describeAccountAttributesResponse :: Int -> DescribeAccountAttributesResponse
describeAccountAttributesResponse pStatusCode = DescribeAccountAttributesResponse'{_daarAccountQuotas = Nothing, _daarStatusCode = pStatusCode};

-- | A list of AccountQuota objects. Within this list, each quota has a name,
-- a count of usage toward the quota maximum, and a maximum value for the
-- quota.
daarAccountQuotas :: Lens' DescribeAccountAttributesResponse [AccountQuota]
daarAccountQuotas = lens _daarAccountQuotas (\ s a -> s{_daarAccountQuotas = a}) . _Default;

-- | FIXME: Undocumented member.
daarStatusCode :: Lens' DescribeAccountAttributesResponse Int
daarStatusCode = lens _daarStatusCode (\ s a -> s{_daarStatusCode = a});

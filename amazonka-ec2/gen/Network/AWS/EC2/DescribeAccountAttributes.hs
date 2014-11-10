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

-- Module      : Network.AWS.EC2.DescribeAccountAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of your AWS account.
module Network.AWS.EC2.DescribeAccountAttributes
    (
    -- * Request
      DescribeAccountAttributes
    -- ** Request constructor
    , describeAccountAttributes
    -- ** Request lenses
    , daaAttributeNames
    , daaDryRun

    -- * Response
    , DescribeAccountAttributesResult
    -- ** Response constructor
    , describeAccountAttributesResponse
    -- ** Response lenses
    , daarAccountAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeAccountAttributes = DescribeAccountAttributes
    { _daaAttributeNames :: [Text]
    , _daaDryRun         :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAccountAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaAttributeNames' @::@ ['Text']
--
-- * 'daaDryRun' @::@ 'Maybe' 'Bool'
--
describeAccountAttributes :: DescribeAccountAttributes
describeAccountAttributes = DescribeAccountAttributes
    { _daaDryRun         = Nothing
    , _daaAttributeNames = mempty
    }

-- | One or more account attribute names.
daaAttributeNames :: Lens' DescribeAccountAttributes [Text]
daaAttributeNames =
    lens _daaAttributeNames (\s a -> s { _daaAttributeNames = a })

daaDryRun :: Lens' DescribeAccountAttributes (Maybe Bool)
daaDryRun = lens _daaDryRun (\s a -> s { _daaDryRun = a })

instance ToPath DescribeAccountAttributes where
    toPath = const "/"

instance ToQuery DescribeAccountAttributes

newtype DescribeAccountAttributesResult = DescribeAccountAttributesResult
    { _daarAccountAttributes :: [AccountAttribute]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeAccountAttributesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daarAccountAttributes' @::@ ['AccountAttribute']
--
describeAccountAttributesResponse :: DescribeAccountAttributesResult
describeAccountAttributesResponse = DescribeAccountAttributesResult
    { _daarAccountAttributes = mempty
    }

-- | Information about one or more account attributes.
daarAccountAttributes :: Lens' DescribeAccountAttributesResult [AccountAttribute]
daarAccountAttributes =
    lens _daarAccountAttributes (\s a -> s { _daarAccountAttributes = a })

instance AWSRequest DescribeAccountAttributes where
    type Sv DescribeAccountAttributes = EC2
    type Rs DescribeAccountAttributes = DescribeAccountAttributesResult

    request  = post "DescribeAccountAttributes"
    response = xmlResponse $ \h x -> DescribeAccountAttributesResult
        <$> x %| "accountAttributeSet"

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

-- Module      : Network.AWS.EC2.DescribeAccountAttributes
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

-- | Describes the specified attribute of your AWS account.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAccountAttributes.html>
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
    , DescribeAccountAttributesResponse
    -- ** Response constructor
    , describeAccountAttributesResponse
    -- ** Response lenses
    , daarAccountAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeAccountAttributes = DescribeAccountAttributes
    { _daaAttributeNames :: List "attributeName" AccountAttributeName
    , _daaDryRun         :: Maybe Bool
    } deriving (Eq, Show)

-- | 'DescribeAccountAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaAttributeNames' @::@ ['AccountAttributeName']
--
-- * 'daaDryRun' @::@ 'Maybe' 'Bool'
--
describeAccountAttributes :: DescribeAccountAttributes
describeAccountAttributes = DescribeAccountAttributes
    { _daaDryRun         = Nothing
    , _daaAttributeNames = mempty
    }

-- | One or more account attribute names.
daaAttributeNames :: Lens' DescribeAccountAttributes [AccountAttributeName]
daaAttributeNames =
    lens _daaAttributeNames (\s a -> s { _daaAttributeNames = a })
        . _List

daaDryRun :: Lens' DescribeAccountAttributes (Maybe Bool)
daaDryRun = lens _daaDryRun (\s a -> s { _daaDryRun = a })

newtype DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { _daarAccountAttributes :: List "item" AccountAttribute
    } deriving (Eq, Show, Monoid, Semigroup)

-- | 'DescribeAccountAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daarAccountAttributes' @::@ ['AccountAttribute']
--
describeAccountAttributesResponse :: DescribeAccountAttributesResponse
describeAccountAttributesResponse = DescribeAccountAttributesResponse
    { _daarAccountAttributes = mempty
    }

-- | Information about one or more account attributes.
daarAccountAttributes :: Lens' DescribeAccountAttributesResponse [AccountAttribute]
daarAccountAttributes =
    lens _daarAccountAttributes (\s a -> s { _daarAccountAttributes = a })
        . _List

instance ToPath DescribeAccountAttributes where
    toPath = const "/"

instance ToQuery DescribeAccountAttributes where
    toQuery DescribeAccountAttributes{..} = mconcat
        [ toQuery        _daaAttributeNames
        , "dryRun"        =? _daaDryRun
        ]

instance ToHeaders DescribeAccountAttributes

instance AWSRequest DescribeAccountAttributes where
    type Sv DescribeAccountAttributes = EC2
    type Rs DescribeAccountAttributes = DescribeAccountAttributesResponse

    request  = post "DescribeAccountAttributes"
    response = xmlResponse

instance FromXML DescribeAccountAttributesResponse where
    parseXML x = DescribeAccountAttributesResponse
        <$> parseXML x

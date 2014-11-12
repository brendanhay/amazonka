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

-- Module      : Network.AWS.CloudSearch.DescribeExpressions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the expressions configured for the search domain. Can be limited to
-- specific expressions by name. By default, shows all expressions and
-- includes any pending changes to the configuration. Set the Deployed option
-- to true to show the active configuration and exclude pending changes. For
-- more information, see Configuring Expressions in the Amazon CloudSearch
-- Developer Guide.
module Network.AWS.CloudSearch.DescribeExpressions
    (
    -- * Request
      DescribeExpressions
    -- ** Request constructor
    , describeExpressions
    -- ** Request lenses
    , de1Deployed
    , de1DomainName
    , de1ExpressionNames

    -- * Response
    , DescribeExpressionsResponse
    -- ** Response constructor
    , describeExpressionsResponse
    -- ** Response lenses
    , derExpressions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types

data DescribeExpressions = DescribeExpressions
    { _de1Deployed        :: Maybe Bool
    , _de1DomainName      :: Text
    , _de1ExpressionNames :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeExpressions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'de1Deployed' @::@ 'Maybe' 'Bool'
--
-- * 'de1DomainName' @::@ 'Text'
--
-- * 'de1ExpressionNames' @::@ ['Text']
--
describeExpressions :: Text -- ^ 'de1DomainName'
                    -> DescribeExpressions
describeExpressions p1 = DescribeExpressions
    { _de1DomainName      = p1
    , _de1ExpressionNames = mempty
    , _de1Deployed        = Nothing
    }

-- | Whether to display the deployed configuration (true) or include any
-- pending changes (false). Defaults to false.
de1Deployed :: Lens' DescribeExpressions (Maybe Bool)
de1Deployed = lens _de1Deployed (\s a -> s { _de1Deployed = a })

-- | The name of the domain you want to describe.
de1DomainName :: Lens' DescribeExpressions Text
de1DomainName = lens _de1DomainName (\s a -> s { _de1DomainName = a })

-- | Limits the DescribeExpressions response to the specified expressions. If
-- not specified, all expressions are shown.
de1ExpressionNames :: Lens' DescribeExpressions [Text]
de1ExpressionNames =
    lens _de1ExpressionNames (\s a -> s { _de1ExpressionNames = a })

instance ToQuery DescribeExpressions

instance ToPath DescribeExpressions where
    toPath = const "/"

newtype DescribeExpressionsResponse = DescribeExpressionsResponse
    { _derExpressions :: [ExpressionStatus]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeExpressionsResponse where
    type Item DescribeExpressionsResponse = ExpressionStatus

    fromList = DescribeExpressionsResponse . fromList
    toList   = toList . _derExpressions

-- | 'DescribeExpressionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derExpressions' @::@ ['ExpressionStatus']
--
describeExpressionsResponse :: DescribeExpressionsResponse
describeExpressionsResponse = DescribeExpressionsResponse
    { _derExpressions = mempty
    }

-- | The expressions configured for the domain.
derExpressions :: Lens' DescribeExpressionsResponse [ExpressionStatus]
derExpressions = lens _derExpressions (\s a -> s { _derExpressions = a })

instance FromXML DescribeExpressionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeExpressionsResponse"

instance AWSRequest DescribeExpressions where
    type Sv DescribeExpressions = CloudSearch
    type Rs DescribeExpressions = DescribeExpressionsResponse

    request  = post "DescribeExpressions"
    response = xmlResponse $ \h x -> DescribeExpressionsResponse
        <$> x %| "Expressions"

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
    , deDeployed
    , deDomainName
    , deExpressionNames

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
import qualified GHC.Exts

data DescribeExpressions = DescribeExpressions
    { _deDeployed        :: Maybe Bool
    , _deDomainName      :: Text
    , _deExpressionNames :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeExpressions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deDeployed' @::@ 'Maybe' 'Bool'
--
-- * 'deDomainName' @::@ 'Text'
--
-- * 'deExpressionNames' @::@ ['Text']
--
describeExpressions :: Text -- ^ 'deDomainName'
                    -> DescribeExpressions
describeExpressions p1 = DescribeExpressions
    { _deDomainName      = p1
    , _deExpressionNames = mempty
    , _deDeployed        = Nothing
    }

-- | Whether to display the deployed configuration (true) or include any
-- pending changes (false). Defaults to false.
deDeployed :: Lens' DescribeExpressions (Maybe Bool)
deDeployed = lens _deDeployed (\s a -> s { _deDeployed = a })

-- | The name of the domain you want to describe.
deDomainName :: Lens' DescribeExpressions Text
deDomainName = lens _deDomainName (\s a -> s { _deDomainName = a })

-- | Limits the DescribeExpressions response to the specified expressions. If
-- not specified, all expressions are shown.
deExpressionNames :: Lens' DescribeExpressions [Text]
deExpressionNames =
    lens _deExpressionNames (\s a -> s { _deExpressionNames = a })

instance ToQuery DescribeExpressions

instance ToPath DescribeExpressions where
    toPath = const "/"

newtype DescribeExpressionsResponse = DescribeExpressionsResponse
    { _derExpressions :: [ExpressionStatus]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeExpressionsResponse where
    type Item DescribeExpressionsResponse = ExpressionStatus

    fromList = DescribeExpressionsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _derExpressions

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

instance AWSRequest DescribeExpressions where
    type Sv DescribeExpressions = CloudSearch
    type Rs DescribeExpressions = DescribeExpressionsResponse

    request  = post "DescribeExpressions"
    response = xmlResponse $ \h x -> DescribeExpressionsResponse
        <$> x %| "Expressions"

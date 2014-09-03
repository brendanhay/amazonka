{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DescribeExpressions
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
module Network.AWS.CloudSearch.V2013_01_01.DescribeExpressions
    (
    -- * Request
      DescribeExpressions
    -- ** Request constructor
    , describeExpressions
    -- ** Request lenses
    , devDomainName
    , devDeployed
    , devExpressionNames

    -- * Response
    , DescribeExpressionsResponse
    -- ** Response lenses
    , dewExpressions
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeExpressions' request.
describeExpressions :: Text -- ^ 'devDomainName'
                    -> DescribeExpressions
describeExpressions p1 = DescribeExpressions
    { _devDomainName = p1
    , _devDeployed = Nothing
    , _devExpressionNames = mempty
    }

data DescribeExpressions = DescribeExpressions
    { _devDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _devDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    , _devExpressionNames :: [Text]
      -- ^ Limits the DescribeExpressions response to the specified
      -- expressions. If not specified, all expressions are shown.
    } deriving (Show, Generic)

-- | The name of the domain you want to describe.
devDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeExpressions
    -> f DescribeExpressions
devDomainName f x =
    (\y -> x { _devDomainName = y })
       <$> f (_devDomainName x)
{-# INLINE devDomainName #-}

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
devDeployed
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeExpressions
    -> f DescribeExpressions
devDeployed f x =
    (\y -> x { _devDeployed = y })
       <$> f (_devDeployed x)
{-# INLINE devDeployed #-}

-- | Limits the DescribeExpressions response to the specified expressions. If
-- not specified, all expressions are shown.
devExpressionNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeExpressions
    -> f DescribeExpressions
devExpressionNames f x =
    (\y -> x { _devExpressionNames = y })
       <$> f (_devExpressionNames x)
{-# INLINE devExpressionNames #-}

instance ToQuery DescribeExpressions where
    toQuery = genericQuery def

data DescribeExpressionsResponse = DescribeExpressionsResponse
    { _dewExpressions :: [ExpressionStatus]
      -- ^ The expressions configured for the domain.
    } deriving (Show, Generic)

-- | The expressions configured for the domain.
dewExpressions
    :: Functor f
    => ([ExpressionStatus]
    -> f ([ExpressionStatus]))
    -> DescribeExpressionsResponse
    -> f DescribeExpressionsResponse
dewExpressions f x =
    (\y -> x { _dewExpressions = y })
       <$> f (_dewExpressions x)
{-# INLINE dewExpressions #-}

instance FromXML DescribeExpressionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeExpressions where
    type Sv DescribeExpressions = CloudSearch
    type Rs DescribeExpressions = DescribeExpressionsResponse

    request = post "DescribeExpressions"
    response _ = xmlResponse

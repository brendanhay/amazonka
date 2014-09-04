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
    , mkDescribeExpressionsRequest
    -- ** Request lenses
    , devDomainName
    , devExpressionNames
    , devDeployed

    -- * Response
    , DescribeExpressionsResponse
    -- ** Response lenses
    , dewExpressions
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeExpressions' request.
mkDescribeExpressionsRequest :: Text -- ^ 'devDomainName'
                             -> DescribeExpressions
mkDescribeExpressionsRequest p1 = DescribeExpressions
    { _devDomainName = p1
    , _devExpressionNames = mempty
    , _devDeployed = Nothing
    }
{-# INLINE mkDescribeExpressionsRequest #-}

data DescribeExpressions = DescribeExpressions
    { _devDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _devExpressionNames :: [Text]
      -- ^ Limits the DescribeExpressions response to the specified
      -- expressions. If not specified, all expressions are shown.
    , _devDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    } deriving (Show, Generic)

-- | The name of the domain you want to describe.
devDomainName :: Lens' DescribeExpressions (Text)
devDomainName = lens _devDomainName (\s a -> s { _devDomainName = a })
{-# INLINE devDomainName #-}

-- | Limits the DescribeExpressions response to the specified expressions. If
-- not specified, all expressions are shown.
devExpressionNames :: Lens' DescribeExpressions ([Text])
devExpressionNames = lens _devExpressionNames (\s a -> s { _devExpressionNames = a })
{-# INLINE devExpressionNames #-}

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
devDeployed :: Lens' DescribeExpressions (Maybe Bool)
devDeployed = lens _devDeployed (\s a -> s { _devDeployed = a })
{-# INLINE devDeployed #-}

instance ToQuery DescribeExpressions where
    toQuery = genericQuery def

newtype DescribeExpressionsResponse = DescribeExpressionsResponse
    { _dewExpressions :: [ExpressionStatus]
      -- ^ The expressions configured for the domain.
    } deriving (Show, Generic)

-- | The expressions configured for the domain.
dewExpressions :: Lens' DescribeExpressionsResponse ([ExpressionStatus])
dewExpressions = lens _dewExpressions (\s a -> s { _dewExpressions = a })
{-# INLINE dewExpressions #-}

instance FromXML DescribeExpressionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeExpressions where
    type Sv DescribeExpressions = CloudSearch
    type Rs DescribeExpressions = DescribeExpressionsResponse

    request = post "DescribeExpressions"
    response _ = xmlResponse

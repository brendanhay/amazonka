{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.CloudSearch.V2013_01_01.DescribeExpressions where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeExpressions' request.
describeExpressions :: Text -- ^ '_derDomainName'
                    -> DescribeExpressions
describeExpressions p1 = DescribeExpressions
    { _derDomainName = p1
    , _derDeployed = Nothing
    , _derExpressionNames = mempty
    }

data DescribeExpressions = DescribeExpressions
    { _derDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _derDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    , _derExpressionNames :: [Text]
      -- ^ Limits the DescribeExpressions response to the specified
      -- expressions. If not specified, all expressions are shown.
    } deriving (Show, Generic)

makeLenses ''DescribeExpressions

instance ToQuery DescribeExpressions where
    toQuery = genericToQuery def

data DescribeExpressionsResponse = DescribeExpressionsResponse
    { _desExpressions :: [ExpressionStatus]
      -- ^ The expressions configured for the domain.
    } deriving (Show, Generic)

makeLenses ''DescribeExpressionsResponse

instance AWSRequest DescribeExpressions where
    type Sv DescribeExpressions = CloudSearch
    type Rs DescribeExpressions = DescribeExpressionsResponse

    request = post "DescribeExpressions"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeExpressionsResponse
            <*> xml %| "ExpressionStatusList"

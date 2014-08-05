{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DefineExpression
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures an Expression for the search domain. Used to create new
-- expressions and modify existing ones. If the expression exists, the new
-- configuration replaces the old one. For more information, see Configuring
-- Expressions in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DefineExpression where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

data DefineExpression = DefineExpression
    { _detDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _detExpression :: Expression
      -- ^ A named expression that can be evaluated at search time. Can be
      -- used for sorting and filtering search results and constructing
      -- other expressions.
    } deriving (Show, Generic)

makeLenses ''DefineExpression

instance ToQuery DefineExpression where
    toQuery = genericToQuery def

data DefineExpressionResponse = DefineExpressionResponse
    { _deuExpression :: ExpressionStatus
      -- ^ The value of an Expression and its current status.
    } deriving (Show, Generic)

makeLenses ''DefineExpressionResponse

instance AWSRequest DefineExpression where
    type Sv DefineExpression = CloudSearch
    type Rs DefineExpression = DefineExpressionResponse

    request = post "DefineExpression"
    response _ = cursorResponse $ \hs xml ->
        pure DefineExpressionResponse
            <*> xml %| "ExpressionStatus"

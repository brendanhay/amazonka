{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CloudSearch.V2013_01_01.DefineExpression
    (
    -- * Request
      DefineExpression
    -- ** Request constructor
    , mkDefineExpressionRequest
    -- ** Request lenses
    , derDomainName
    , derExpression

    -- * Response
    , DefineExpressionResponse
    -- ** Response lenses
    , desExpression
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineExpression' request.
mkDefineExpressionRequest :: Text -- ^ 'derDomainName'
                          -> Expression -- ^ 'derExpression'
                          -> DefineExpression
mkDefineExpressionRequest p1 p2 = DefineExpression
    { _derDomainName = p1
    , _derExpression = p2
    }
{-# INLINE mkDefineExpressionRequest #-}

data DefineExpression = DefineExpression
    { _derDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _derExpression :: Expression
      -- ^ A named expression that can be evaluated at search time. Can be
      -- used for sorting and filtering search results and constructing
      -- other expressions.
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
derDomainName :: Lens' DefineExpression (Text)
derDomainName = lens _derDomainName (\s a -> s { _derDomainName = a })
{-# INLINE derDomainName #-}

-- | A named expression that can be evaluated at search time. Can be used for
-- sorting and filtering search results and constructing other expressions.
derExpression :: Lens' DefineExpression (Expression)
derExpression = lens _derExpression (\s a -> s { _derExpression = a })
{-# INLINE derExpression #-}

instance ToQuery DefineExpression where
    toQuery = genericQuery def

newtype DefineExpressionResponse = DefineExpressionResponse
    { _desExpression :: ExpressionStatus
      -- ^ The value of an Expression and its current status.
    } deriving (Show, Generic)

-- | The value of an Expression and its current status.
desExpression :: Lens' DefineExpressionResponse (ExpressionStatus)
desExpression = lens _desExpression (\s a -> s { _desExpression = a })
{-# INLINE desExpression #-}

instance FromXML DefineExpressionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineExpression where
    type Sv DefineExpression = CloudSearch
    type Rs DefineExpression = DefineExpressionResponse

    request = post "DefineExpression"
    response _ = xmlResponse

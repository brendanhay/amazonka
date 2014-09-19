{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DefineExpression
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
module Network.AWS.CloudSearch.DefineExpression
    (
    -- * Request
      DefineExpression
    -- ** Request constructor
    , defineExpression
    -- ** Request lenses
    , deDomainName
    , deExpression

    -- * Response
    , DefineExpressionResponse
    -- ** Response constructor
    , defineExpressionResponse
    -- ** Response lenses
    , derExpression
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DefineExpression operation. Specifies
-- the name of the domain you want to update and the expression you want to
-- configure.
data DefineExpression = DefineExpression
    { _deDomainName :: Text
    , _deExpression :: Expression
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineExpression' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @Expression ::@ @Expression@
--
defineExpression :: Text -- ^ 'deDomainName'
                 -> Expression -- ^ 'deExpression'
                 -> DefineExpression
defineExpression p1 p2 = DefineExpression
    { _deDomainName = p1
    , _deExpression = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
deDomainName :: Lens' DefineExpression Text
deDomainName = lens _deDomainName (\s a -> s { _deDomainName = a })

-- | A named expression that can be evaluated at search time. Can be used for
-- sorting and filtering search results and constructing other expressions.
deExpression :: Lens' DefineExpression Expression
deExpression = lens _deExpression (\s a -> s { _deExpression = a })

instance ToQuery DefineExpression where
    toQuery = genericQuery def

-- | The result of a DefineExpression request. Contains the status of the
-- newly-configured expression.
newtype DefineExpressionResponse = DefineExpressionResponse
    { _derExpression :: ExpressionStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineExpressionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Expression ::@ @ExpressionStatus@
--
defineExpressionResponse :: ExpressionStatus -- ^ 'derExpression'
                         -> DefineExpressionResponse
defineExpressionResponse p1 = DefineExpressionResponse
    { _derExpression = p1
    }

-- | The value of an Expression and its current status.
derExpression :: Lens' DefineExpressionResponse ExpressionStatus
derExpression = lens _derExpression (\s a -> s { _derExpression = a })

instance FromXML DefineExpressionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineExpression where
    type Sv DefineExpression = CloudSearch
    type Rs DefineExpression = DefineExpressionResponse

    request = post "DefineExpression"
    response _ = xmlResponse

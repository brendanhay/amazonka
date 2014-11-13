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
    , de1DomainName
    , de1Expression

    -- * Response
    , DefineExpressionResponse
    -- ** Response constructor
    , defineExpressionResponse
    -- ** Response lenses
    , derExpression
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DefineExpression = DefineExpression
    { _de1DomainName :: Text
    , _de1Expression :: Expression
    } deriving (Eq, Show, Generic)

-- | 'DefineExpression' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'de1DomainName' @::@ 'Text'
--
-- * 'de1Expression' @::@ 'Expression'
--
defineExpression :: Text -- ^ 'de1DomainName'
                 -> Expression -- ^ 'de1Expression'
                 -> DefineExpression
defineExpression p1 p2 = DefineExpression
    { _de1DomainName = p1
    , _de1Expression = p2
    }

de1DomainName :: Lens' DefineExpression Text
de1DomainName = lens _de1DomainName (\s a -> s { _de1DomainName = a })

de1Expression :: Lens' DefineExpression Expression
de1Expression = lens _de1Expression (\s a -> s { _de1Expression = a })

instance ToQuery DefineExpression

instance ToPath DefineExpression where
    toPath = const "/"

newtype DefineExpressionResponse = DefineExpressionResponse
    { _derExpression :: ExpressionStatus
    } deriving (Eq, Show, Generic)

-- | 'DefineExpressionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derExpression' @::@ 'ExpressionStatus'
--
defineExpressionResponse :: ExpressionStatus -- ^ 'derExpression'
                         -> DefineExpressionResponse
defineExpressionResponse p1 = DefineExpressionResponse
    { _derExpression = p1
    }

derExpression :: Lens' DefineExpressionResponse ExpressionStatus
derExpression = lens _derExpression (\s a -> s { _derExpression = a })

instance AWSRequest DefineExpression where
    type Sv DefineExpression = CloudSearch
    type Rs DefineExpression = DefineExpressionResponse

    request  = post "DefineExpression"
    response = xmlResponse $ \h x -> DefineExpressionResponse
        <$> x %| "Expression"

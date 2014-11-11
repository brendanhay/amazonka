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

-- Module      : Network.AWS.CloudSearch.DeleteExpression
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes an Expression from the search domain. For more information, see
-- Configuring Expressions in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.DeleteExpression
    (
    -- * Request
      DeleteExpression
    -- ** Request constructor
    , deleteExpression
    -- ** Request lenses
    , deDomainName
    , deExpressionName

    -- * Response
    , DeleteExpressionResponse
    -- ** Response constructor
    , deleteExpressionResponse
    -- ** Response lenses
    , der1Expression
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types

data DeleteExpression = DeleteExpression
    { _deDomainName     :: Text
    , _deExpressionName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteExpression' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deDomainName' @::@ 'Text'
--
-- * 'deExpressionName' @::@ 'Text'
--
deleteExpression :: Text -- ^ 'deDomainName'
                 -> Text -- ^ 'deExpressionName'
                 -> DeleteExpression
deleteExpression p1 p2 = DeleteExpression
    { _deDomainName     = p1
    , _deExpressionName = p2
    }

deDomainName :: Lens' DeleteExpression Text
deDomainName = lens _deDomainName (\s a -> s { _deDomainName = a })

-- | The name of the Expression to delete.
deExpressionName :: Lens' DeleteExpression Text
deExpressionName = lens _deExpressionName (\s a -> s { _deExpressionName = a })
instance ToQuery DeleteExpression

instance ToPath DeleteExpression where
    toPath = const "/"

newtype DeleteExpressionResponse = DeleteExpressionResponse
    { _der1Expression :: ExpressionStatus
    } deriving (Eq, Show, Generic)

-- | 'DeleteExpressionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'der1Expression' @::@ 'ExpressionStatus'
--
deleteExpressionResponse :: ExpressionStatus -- ^ 'der1Expression'
                         -> DeleteExpressionResponse
deleteExpressionResponse p1 = DeleteExpressionResponse
    { _der1Expression = p1
    }

-- | The status of the expression being deleted.
der1Expression :: Lens' DeleteExpressionResponse ExpressionStatus
der1Expression = lens _der1Expression (\s a -> s { _der1Expression = a })
instance FromXML DeleteExpressionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteExpressionResponse"

instance AWSRequest DeleteExpression where
    type Sv DeleteExpression = CloudSearch
    type Rs DeleteExpression = DeleteExpressionResponse

    request  = post "DeleteExpression"
    response = xmlResponse $ \h x -> DeleteExpressionResponse
        <$> x %| "Expression"

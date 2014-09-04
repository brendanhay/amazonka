{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DeleteExpression
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
module Network.AWS.CloudSearch.V2013_01_01.DeleteExpression
    (
    -- * Request
      DeleteExpression
    -- ** Request constructor
    , mkDeleteExpressionRequest
    -- ** Request lenses
    , detDomainName
    , detExpressionName

    -- * Response
    , DeleteExpressionResponse
    -- ** Response lenses
    , deuExpression
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteExpression' request.
mkDeleteExpressionRequest :: Text -- ^ 'detDomainName'
                          -> Text -- ^ 'detExpressionName'
                          -> DeleteExpression
mkDeleteExpressionRequest p1 p2 = DeleteExpression
    { _detDomainName = p1
    , _detExpressionName = p2
    }
{-# INLINE mkDeleteExpressionRequest #-}

data DeleteExpression = DeleteExpression
    { _detDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _detExpressionName :: Text
      -- ^ The name of the Expression to delete.
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
detDomainName :: Lens' DeleteExpression (Text)
detDomainName = lens _detDomainName (\s a -> s { _detDomainName = a })
{-# INLINE detDomainName #-}

-- | The name of the Expression to delete.
detExpressionName :: Lens' DeleteExpression (Text)
detExpressionName = lens _detExpressionName (\s a -> s { _detExpressionName = a })
{-# INLINE detExpressionName #-}

instance ToQuery DeleteExpression where
    toQuery = genericQuery def

newtype DeleteExpressionResponse = DeleteExpressionResponse
    { _deuExpression :: ExpressionStatus
      -- ^ The status of the expression being deleted.
    } deriving (Show, Generic)

-- | The status of the expression being deleted.
deuExpression :: Lens' DeleteExpressionResponse (ExpressionStatus)
deuExpression = lens _deuExpression (\s a -> s { _deuExpression = a })
{-# INLINE deuExpression #-}

instance FromXML DeleteExpressionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteExpression where
    type Sv DeleteExpression = CloudSearch
    type Rs DeleteExpression = DeleteExpressionResponse

    request = post "DeleteExpression"
    response _ = xmlResponse

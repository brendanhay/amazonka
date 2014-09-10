{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , mkDescribeExpressions
    -- ** Request lenses
    , de2DomainName
    , de2ExpressionNames
    , de2Deployed

    -- * Response
    , DescribeExpressionsResponse
    -- ** Response constructor
    , mkDescribeExpressionsResponse
    -- ** Response lenses
    , der1Expressions
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeDomains operation. Specifies
-- the name of the domain you want to describe. To restrict the response to
-- particular expressions, specify the names of the expressions you want to
-- describe. To show the active configuration and exclude any pending changes,
-- set the Deployed option to true.
data DescribeExpressions = DescribeExpressions
    { _de2DomainName :: !Text
    , _de2ExpressionNames :: [Text]
    , _de2Deployed :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeExpressions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @ExpressionNames ::@ @[Text]@
--
-- * @Deployed ::@ @Maybe Bool@
--
mkDescribeExpressions :: Text -- ^ 'de2DomainName'
                      -> DescribeExpressions
mkDescribeExpressions p1 = DescribeExpressions
    { _de2DomainName = p1
    , _de2ExpressionNames = mempty
    , _de2Deployed = Nothing
    }

-- | The name of the domain you want to describe.
de2DomainName :: Lens' DescribeExpressions Text
de2DomainName = lens _de2DomainName (\s a -> s { _de2DomainName = a })

-- | Limits the DescribeExpressions response to the specified expressions. If
-- not specified, all expressions are shown.
de2ExpressionNames :: Lens' DescribeExpressions [Text]
de2ExpressionNames =
    lens _de2ExpressionNames (\s a -> s { _de2ExpressionNames = a })

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
de2Deployed :: Lens' DescribeExpressions (Maybe Bool)
de2Deployed = lens _de2Deployed (\s a -> s { _de2Deployed = a })

instance ToQuery DescribeExpressions where
    toQuery = genericQuery def

-- | The result of a DescribeExpressions request. Contains the expressions
-- configured for the domain specified in the request.
newtype DescribeExpressionsResponse = DescribeExpressionsResponse
    { _der1Expressions :: [ExpressionStatus]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeExpressionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Expressions ::@ @[ExpressionStatus]@
--
mkDescribeExpressionsResponse :: [ExpressionStatus] -- ^ 'der1Expressions'
                              -> DescribeExpressionsResponse
mkDescribeExpressionsResponse p1 = DescribeExpressionsResponse
    { _der1Expressions = p1
    }

-- | The expressions configured for the domain.
der1Expressions :: Lens' DescribeExpressionsResponse [ExpressionStatus]
der1Expressions = lens _der1Expressions (\s a -> s { _der1Expressions = a })

instance FromXML DescribeExpressionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeExpressions where
    type Sv DescribeExpressions = CloudSearch
    type Rs DescribeExpressions = DescribeExpressionsResponse

    request = post "DescribeExpressions"
    response _ = xmlResponse

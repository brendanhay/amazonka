{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.DescribeExpressions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets the expressions configured for the search domain. Can be limited to
-- specific expressions by name. By default, shows all expressions and
-- includes any pending changes to the configuration. Set the @Deployed@
-- option to @true@ to show the active configuration and exclude pending
-- changes. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeExpressions.html>
module Network.AWS.CloudSearch.DescribeExpressions
    (
    -- * Request
      DescribeExpressions
    -- ** Request constructor
    , describeExpressions
    -- ** Request lenses
    , deDeployed
    , deExpressionNames
    , deDomainName

    -- * Response
    , DescribeExpressionsResponse
    -- ** Response constructor
    , describeExpressionsResponse
    -- ** Response lenses
    , desExpressions
    , desStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DescribeDomains@ operation.
-- Specifies the name of the domain you want to describe. To restrict the
-- response to particular expressions, specify the names of the expressions
-- you want to describe. To show the active configuration and exclude any
-- pending changes, set the @Deployed@ option to @true@.
--
-- /See:/ 'describeExpressions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deDeployed'
--
-- * 'deExpressionNames'
--
-- * 'deDomainName'
data DescribeExpressions = DescribeExpressions'
    { _deDeployed        :: Maybe Bool
    , _deExpressionNames :: Maybe [Text]
    , _deDomainName      :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeExpressions' smart constructor.
describeExpressions :: Text -> DescribeExpressions
describeExpressions pDomainName =
    DescribeExpressions'
    { _deDeployed = Nothing
    , _deExpressionNames = Nothing
    , _deDomainName = pDomainName
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
deDeployed :: Lens' DescribeExpressions (Maybe Bool)
deDeployed = lens _deDeployed (\ s a -> s{_deDeployed = a});

-- | Limits the @DescribeExpressions@ response to the specified expressions.
-- If not specified, all expressions are shown.
deExpressionNames :: Lens' DescribeExpressions [Text]
deExpressionNames = lens _deExpressionNames (\ s a -> s{_deExpressionNames = a}) . _Default;

-- | The name of the domain you want to describe.
deDomainName :: Lens' DescribeExpressions Text
deDomainName = lens _deDomainName (\ s a -> s{_deDomainName = a});

instance AWSRequest DescribeExpressions where
        type Sv DescribeExpressions = CloudSearch
        type Rs DescribeExpressions =
             DescribeExpressionsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeExpressionsResult"
              (\ s h x ->
                 DescribeExpressionsResponse' <$>
                   (x .@? "Expressions" .!@ mempty >>=
                      parseXMLList "member")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeExpressions where
        toHeaders = const mempty

instance ToPath DescribeExpressions where
        toPath = const "/"

instance ToQuery DescribeExpressions where
        toQuery DescribeExpressions'{..}
          = mconcat
              ["Action" =: ("DescribeExpressions" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "Deployed" =: _deDeployed,
               "ExpressionNames" =:
                 toQuery
                   (toQueryList "member" <$> _deExpressionNames),
               "DomainName" =: _deDomainName]

-- | The result of a @DescribeExpressions@ request. Contains the expressions
-- configured for the domain specified in the request.
--
-- /See:/ 'describeExpressionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desExpressions'
--
-- * 'desStatus'
data DescribeExpressionsResponse = DescribeExpressionsResponse'
    { _desExpressions :: [ExpressionStatus]
    , _desStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeExpressionsResponse' smart constructor.
describeExpressionsResponse :: Int -> DescribeExpressionsResponse
describeExpressionsResponse pStatus =
    DescribeExpressionsResponse'
    { _desExpressions = mempty
    , _desStatus = pStatus
    }

-- | The expressions configured for the domain.
desExpressions :: Lens' DescribeExpressionsResponse [ExpressionStatus]
desExpressions = lens _desExpressions (\ s a -> s{_desExpressions = a});

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeExpressionsResponse Int
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});

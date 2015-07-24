{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeExpressions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the expressions configured for the search domain. Can be limited to
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
    , drsStatus
    , drsExpressions
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
    { _deDeployed        :: !(Maybe Bool)
    , _deExpressionNames :: !(Maybe [Text])
    , _deDomainName      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeExpressions' smart constructor.
describeExpressions :: Text -> DescribeExpressions
describeExpressions pDomainName_ =
    DescribeExpressions'
    { _deDeployed = Nothing
    , _deExpressionNames = Nothing
    , _deDomainName = pDomainName_
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
        request = post "DescribeExpressions"
        response
          = receiveXMLWrapper "DescribeExpressionsResult"
              (\ s h x ->
                 DescribeExpressionsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "Expressions" .!@ mempty >>=
                        parseXMLList "member"))

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
-- * 'drsStatus'
--
-- * 'drsExpressions'
data DescribeExpressionsResponse = DescribeExpressionsResponse'
    { _drsStatus      :: !Int
    , _drsExpressions :: ![ExpressionStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeExpressionsResponse' smart constructor.
describeExpressionsResponse :: Int -> DescribeExpressionsResponse
describeExpressionsResponse pStatus_ =
    DescribeExpressionsResponse'
    { _drsStatus = pStatus_
    , _drsExpressions = mempty
    }

-- | FIXME: Undocumented member.
drsStatus :: Lens' DescribeExpressionsResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});

-- | The expressions configured for the domain.
drsExpressions :: Lens' DescribeExpressionsResponse [ExpressionStatus]
drsExpressions = lens _drsExpressions (\ s a -> s{_drsExpressions = a});

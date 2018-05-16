{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeExpressions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the expressions configured for the search domain. Can be limited to specific expressions by name. By default, shows all expressions and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-expressions.html Configuring Expressions> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DescribeExpressions
    (
    -- * Creating a Request
      describeExpressions
    , DescribeExpressions
    -- * Request Lenses
    , deDeployed
    , deExpressionNames
    , deDomainName

    -- * Destructuring the Response
    , describeExpressionsResponse
    , DescribeExpressionsResponse
    -- * Response Lenses
    , drsResponseStatus
    , drsExpressions
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DescribeDomains' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular expressions, specify the names of the expressions you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
--
--
-- /See:/ 'describeExpressions' smart constructor.
data DescribeExpressions = DescribeExpressions'
  { _deDeployed        :: !(Maybe Bool)
  , _deExpressionNames :: !(Maybe [Text])
  , _deDomainName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeExpressions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deDeployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- * 'deExpressionNames' - Limits the @'DescribeExpressions' @ response to the specified expressions. If not specified, all expressions are shown.
--
-- * 'deDomainName' - The name of the domain you want to describe.
describeExpressions
    :: Text -- ^ 'deDomainName'
    -> DescribeExpressions
describeExpressions pDomainName_ =
  DescribeExpressions'
    { _deDeployed = Nothing
    , _deExpressionNames = Nothing
    , _deDomainName = pDomainName_
    }


-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
deDeployed :: Lens' DescribeExpressions (Maybe Bool)
deDeployed = lens _deDeployed (\ s a -> s{_deDeployed = a})

-- | Limits the @'DescribeExpressions' @ response to the specified expressions. If not specified, all expressions are shown.
deExpressionNames :: Lens' DescribeExpressions [Text]
deExpressionNames = lens _deExpressionNames (\ s a -> s{_deExpressionNames = a}) . _Default . _Coerce

-- | The name of the domain you want to describe.
deDomainName :: Lens' DescribeExpressions Text
deDomainName = lens _deDomainName (\ s a -> s{_deDomainName = a})

instance AWSRequest DescribeExpressions where
        type Rs DescribeExpressions =
             DescribeExpressionsResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DescribeExpressionsResult"
              (\ s h x ->
                 DescribeExpressionsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "Expressions" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable DescribeExpressions where

instance NFData DescribeExpressions where

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

-- | The result of a @DescribeExpressions@ request. Contains the expressions configured for the domain specified in the request.
--
--
--
-- /See:/ 'describeExpressionsResponse' smart constructor.
data DescribeExpressionsResponse = DescribeExpressionsResponse'
  { _drsResponseStatus :: !Int
  , _drsExpressions    :: ![ExpressionStatus]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeExpressionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsExpressions' - The expressions configured for the domain.
describeExpressionsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeExpressionsResponse
describeExpressionsResponse pResponseStatus_ =
  DescribeExpressionsResponse'
    {_drsResponseStatus = pResponseStatus_, _drsExpressions = mempty}


-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeExpressionsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

-- | The expressions configured for the domain.
drsExpressions :: Lens' DescribeExpressionsResponse [ExpressionStatus]
drsExpressions = lens _drsExpressions (\ s a -> s{_drsExpressions = a}) . _Coerce

instance NFData DescribeExpressionsResponse where

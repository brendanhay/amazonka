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
-- Module      : Network.AWS.CloudSearch.DescribeIndexFields
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the index fields configured for the search domain. Can be limited to specific fields by name. By default, shows all fields and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Domain Information> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DescribeIndexFields
    (
    -- * Creating a Request
      describeIndexFields
    , DescribeIndexFields
    -- * Request Lenses
    , difDeployed
    , difFieldNames
    , difDomainName

    -- * Destructuring the Response
    , describeIndexFieldsResponse
    , DescribeIndexFieldsResponse
    -- * Response Lenses
    , difsrsResponseStatus
    , difsrsIndexFields
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DescribeIndexFields' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular index fields, specify the names of the index fields you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
--
--
-- /See:/ 'describeIndexFields' smart constructor.
data DescribeIndexFields = DescribeIndexFields'
  { _difDeployed   :: !(Maybe Bool)
  , _difFieldNames :: !(Maybe [Text])
  , _difDomainName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIndexFields' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'difDeployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- * 'difFieldNames' - A list of the index fields you want to describe. If not specified, information is returned for all configured index fields.
--
-- * 'difDomainName' - The name of the domain you want to describe.
describeIndexFields
    :: Text -- ^ 'difDomainName'
    -> DescribeIndexFields
describeIndexFields pDomainName_ =
  DescribeIndexFields'
    { _difDeployed = Nothing
    , _difFieldNames = Nothing
    , _difDomainName = pDomainName_
    }


-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
difDeployed :: Lens' DescribeIndexFields (Maybe Bool)
difDeployed = lens _difDeployed (\ s a -> s{_difDeployed = a})

-- | A list of the index fields you want to describe. If not specified, information is returned for all configured index fields.
difFieldNames :: Lens' DescribeIndexFields [Text]
difFieldNames = lens _difFieldNames (\ s a -> s{_difFieldNames = a}) . _Default . _Coerce

-- | The name of the domain you want to describe.
difDomainName :: Lens' DescribeIndexFields Text
difDomainName = lens _difDomainName (\ s a -> s{_difDomainName = a})

instance AWSRequest DescribeIndexFields where
        type Rs DescribeIndexFields =
             DescribeIndexFieldsResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DescribeIndexFieldsResult"
              (\ s h x ->
                 DescribeIndexFieldsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "IndexFields" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable DescribeIndexFields where

instance NFData DescribeIndexFields where

instance ToHeaders DescribeIndexFields where
        toHeaders = const mempty

instance ToPath DescribeIndexFields where
        toPath = const "/"

instance ToQuery DescribeIndexFields where
        toQuery DescribeIndexFields'{..}
          = mconcat
              ["Action" =: ("DescribeIndexFields" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "Deployed" =: _difDeployed,
               "FieldNames" =:
                 toQuery (toQueryList "member" <$> _difFieldNames),
               "DomainName" =: _difDomainName]

-- | The result of a @DescribeIndexFields@ request. Contains the index fields configured for the domain specified in the request.
--
--
--
-- /See:/ 'describeIndexFieldsResponse' smart constructor.
data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse'
  { _difsrsResponseStatus :: !Int
  , _difsrsIndexFields    :: ![IndexFieldStatus]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIndexFieldsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'difsrsResponseStatus' - -- | The response status code.
--
-- * 'difsrsIndexFields' - The index fields configured for the domain.
describeIndexFieldsResponse
    :: Int -- ^ 'difsrsResponseStatus'
    -> DescribeIndexFieldsResponse
describeIndexFieldsResponse pResponseStatus_ =
  DescribeIndexFieldsResponse'
    {_difsrsResponseStatus = pResponseStatus_, _difsrsIndexFields = mempty}


-- | -- | The response status code.
difsrsResponseStatus :: Lens' DescribeIndexFieldsResponse Int
difsrsResponseStatus = lens _difsrsResponseStatus (\ s a -> s{_difsrsResponseStatus = a})

-- | The index fields configured for the domain.
difsrsIndexFields :: Lens' DescribeIndexFieldsResponse [IndexFieldStatus]
difsrsIndexFields = lens _difsrsIndexFields (\ s a -> s{_difsrsIndexFields = a}) . _Coerce

instance NFData DescribeIndexFieldsResponse where

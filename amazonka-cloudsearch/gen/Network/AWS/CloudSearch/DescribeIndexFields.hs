{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CloudSearch.DescribeIndexFields
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets information about the index fields configured for the search
-- domain. Can be limited to specific fields by name. By default, shows all
-- fields and includes any pending changes to the configuration. Set the
-- @Deployed@ option to @true@ to show the active configuration and exclude
-- pending changes. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Domain Information>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeIndexFields.html>
module Network.AWS.CloudSearch.DescribeIndexFields
    (
    -- * Request
      DescribeIndexFields
    -- ** Request constructor
    , describeIndexFields
    -- ** Request lenses
    , difDeployed
    , difFieldNames
    , difDomainName

    -- * Response
    , DescribeIndexFieldsResponse
    -- ** Response constructor
    , describeIndexFieldsResponse
    -- ** Response lenses
    , difr1Status
    , difr1IndexFields
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DescribeIndexFields@ operation.
-- Specifies the name of the domain you want to describe. To restrict the
-- response to particular index fields, specify the names of the index
-- fields you want to describe. To show the active configuration and
-- exclude any pending changes, set the @Deployed@ option to @true@.
--
-- /See:/ 'describeIndexFields' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'difDeployed'
--
-- * 'difFieldNames'
--
-- * 'difDomainName'
data DescribeIndexFields = DescribeIndexFields'
    { _difDeployed   :: !(Maybe Bool)
    , _difFieldNames :: !(Maybe [Text])
    , _difDomainName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIndexFields' smart constructor.
describeIndexFields :: Text -> DescribeIndexFields
describeIndexFields pDomainName =
    DescribeIndexFields'
    { _difDeployed = Nothing
    , _difFieldNames = Nothing
    , _difDomainName = pDomainName
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
difDeployed :: Lens' DescribeIndexFields (Maybe Bool)
difDeployed = lens _difDeployed (\ s a -> s{_difDeployed = a});

-- | A list of the index fields you want to describe. If not specified,
-- information is returned for all configured index fields.
difFieldNames :: Lens' DescribeIndexFields [Text]
difFieldNames = lens _difFieldNames (\ s a -> s{_difFieldNames = a}) . _Default;

-- | The name of the domain you want to describe.
difDomainName :: Lens' DescribeIndexFields Text
difDomainName = lens _difDomainName (\ s a -> s{_difDomainName = a});

instance AWSRequest DescribeIndexFields where
        type Sv DescribeIndexFields = CloudSearch
        type Rs DescribeIndexFields =
             DescribeIndexFieldsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeIndexFieldsResult"
              (\ s h x ->
                 DescribeIndexFieldsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "IndexFields" .!@ mempty >>=
                        parseXMLList "member"))

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

-- | The result of a @DescribeIndexFields@ request. Contains the index fields
-- configured for the domain specified in the request.
--
-- /See:/ 'describeIndexFieldsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'difr1Status'
--
-- * 'difr1IndexFields'
data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse'
    { _difr1Status      :: !Int
    , _difr1IndexFields :: ![IndexFieldStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIndexFieldsResponse' smart constructor.
describeIndexFieldsResponse :: Int -> DescribeIndexFieldsResponse
describeIndexFieldsResponse pStatus =
    DescribeIndexFieldsResponse'
    { _difr1Status = pStatus
    , _difr1IndexFields = mempty
    }

-- | FIXME: Undocumented member.
difr1Status :: Lens' DescribeIndexFieldsResponse Int
difr1Status = lens _difr1Status (\ s a -> s{_difr1Status = a});

-- | The index fields configured for the domain.
difr1IndexFields :: Lens' DescribeIndexFieldsResponse [IndexFieldStatus]
difr1IndexFields = lens _difr1IndexFields (\ s a -> s{_difr1IndexFields = a});

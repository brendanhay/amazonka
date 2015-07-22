{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeIndexFields
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the index fields configured for the search
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
    , difrqDeployed
    , difrqFieldNames
    , difrqDomainName

    -- * Response
    , DescribeIndexFieldsResponse
    -- ** Response constructor
    , describeIndexFieldsResponse
    -- ** Response lenses
    , difrsStatus
    , difrsIndexFields
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
-- * 'difrqDeployed'
--
-- * 'difrqFieldNames'
--
-- * 'difrqDomainName'
data DescribeIndexFields = DescribeIndexFields'
    { _difrqDeployed   :: !(Maybe Bool)
    , _difrqFieldNames :: !(Maybe [Text])
    , _difrqDomainName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIndexFields' smart constructor.
describeIndexFields :: Text -> DescribeIndexFields
describeIndexFields pDomainName =
    DescribeIndexFields'
    { _difrqDeployed = Nothing
    , _difrqFieldNames = Nothing
    , _difrqDomainName = pDomainName
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
difrqDeployed :: Lens' DescribeIndexFields (Maybe Bool)
difrqDeployed = lens _difrqDeployed (\ s a -> s{_difrqDeployed = a});

-- | A list of the index fields you want to describe. If not specified,
-- information is returned for all configured index fields.
difrqFieldNames :: Lens' DescribeIndexFields [Text]
difrqFieldNames = lens _difrqFieldNames (\ s a -> s{_difrqFieldNames = a}) . _Default;

-- | The name of the domain you want to describe.
difrqDomainName :: Lens' DescribeIndexFields Text
difrqDomainName = lens _difrqDomainName (\ s a -> s{_difrqDomainName = a});

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
               "Deployed" =: _difrqDeployed,
               "FieldNames" =:
                 toQuery (toQueryList "member" <$> _difrqFieldNames),
               "DomainName" =: _difrqDomainName]

-- | The result of a @DescribeIndexFields@ request. Contains the index fields
-- configured for the domain specified in the request.
--
-- /See:/ 'describeIndexFieldsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'difrsStatus'
--
-- * 'difrsIndexFields'
data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse'
    { _difrsStatus      :: !Int
    , _difrsIndexFields :: ![IndexFieldStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeIndexFieldsResponse' smart constructor.
describeIndexFieldsResponse :: Int -> DescribeIndexFieldsResponse
describeIndexFieldsResponse pStatus =
    DescribeIndexFieldsResponse'
    { _difrsStatus = pStatus
    , _difrsIndexFields = mempty
    }

-- | FIXME: Undocumented member.
difrsStatus :: Lens' DescribeIndexFieldsResponse Int
difrsStatus = lens _difrsStatus (\ s a -> s{_difrsStatus = a});

-- | The index fields configured for the domain.
difrsIndexFields :: Lens' DescribeIndexFieldsResponse [IndexFieldStatus]
difrsIndexFields = lens _difrsIndexFields (\ s a -> s{_difrsIndexFields = a});

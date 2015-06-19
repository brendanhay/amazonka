{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.DescribeServiceAccessPolicies
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

-- | Gets information about the access policies that control access to the
-- domain\'s document and search endpoints. By default, shows the
-- configuration with any pending changes. Set the @Deployed@ option to
-- @true@ to show the active configuration and exclude pending changes. For
-- more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeServiceAccessPolicies.html>
module Network.AWS.CloudSearch.DescribeServiceAccessPolicies
    (
    -- * Request
      DescribeServiceAccessPolicies
    -- ** Request constructor
    , describeServiceAccessPolicies
    -- ** Request lenses
    , dsapDeployed
    , dsapDomainName

    -- * Response
    , DescribeServiceAccessPoliciesResponse
    -- ** Response constructor
    , describeServiceAccessPoliciesResponse
    -- ** Response lenses
    , dsaprAccessPolicies
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeServiceAccessPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsapDeployed'
--
-- * 'dsapDomainName'
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies'{_dsapDeployed :: Maybe Bool, _dsapDomainName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeServiceAccessPolicies' smart constructor.
describeServiceAccessPolicies :: Text -> DescribeServiceAccessPolicies
describeServiceAccessPolicies pDomainName = DescribeServiceAccessPolicies'{_dsapDeployed = Nothing, _dsapDomainName = pDomainName};

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
dsapDeployed :: Lens' DescribeServiceAccessPolicies (Maybe Bool)
dsapDeployed = lens _dsapDeployed (\ s a -> s{_dsapDeployed = a});

-- | The name of the domain you want to describe.
dsapDomainName :: Lens' DescribeServiceAccessPolicies Text
dsapDomainName = lens _dsapDomainName (\ s a -> s{_dsapDomainName = a});

instance AWSRequest DescribeServiceAccessPolicies
         where
        type Sv DescribeServiceAccessPolicies = CloudSearch
        type Rs DescribeServiceAccessPolicies =
             DescribeServiceAccessPoliciesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeServiceAccessPoliciesResult"
              (\ s h x ->
                 DescribeServiceAccessPoliciesResponse' <$>
                   (x .@ "AccessPolicies"))

instance ToHeaders DescribeServiceAccessPolicies
         where
        toHeaders = const mempty

instance ToPath DescribeServiceAccessPolicies where
        toPath = const "/"

instance ToQuery DescribeServiceAccessPolicies where
        toQuery DescribeServiceAccessPolicies'{..}
          = mconcat
              ["Action" =:
                 ("DescribeServiceAccessPolicies" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "Deployed" =: _dsapDeployed,
               "DomainName" =: _dsapDomainName]

-- | /See:/ 'describeServiceAccessPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsaprAccessPolicies'
newtype DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse'{_dsaprAccessPolicies :: AccessPoliciesStatus} deriving (Eq, Read, Show)

-- | 'DescribeServiceAccessPoliciesResponse' smart constructor.
describeServiceAccessPoliciesResponse :: AccessPoliciesStatus -> DescribeServiceAccessPoliciesResponse
describeServiceAccessPoliciesResponse pAccessPolicies = DescribeServiceAccessPoliciesResponse'{_dsaprAccessPolicies = pAccessPolicies};

-- | The access rules configured for the domain specified in the request.
dsaprAccessPolicies :: Lens' DescribeServiceAccessPoliciesResponse AccessPoliciesStatus
dsaprAccessPolicies = lens _dsaprAccessPolicies (\ s a -> s{_dsaprAccessPolicies = a});

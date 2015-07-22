{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeServiceAccessPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the access policies that control access to the
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
    , dsaprqDeployed
    , dsaprqDomainName

    -- * Response
    , DescribeServiceAccessPoliciesResponse
    -- ** Response constructor
    , describeServiceAccessPoliciesResponse
    -- ** Response lenses
    , dsaprsStatus
    , dsaprsAccessPolicies
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DescribeServiceAccessPolicies@
-- operation. Specifies the name of the domain you want to describe. To
-- show the active configuration and exclude any pending changes, set the
-- @Deployed@ option to @true@.
--
-- /See:/ 'describeServiceAccessPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsaprqDeployed'
--
-- * 'dsaprqDomainName'
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies'
    { _dsaprqDeployed   :: !(Maybe Bool)
    , _dsaprqDomainName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServiceAccessPolicies' smart constructor.
describeServiceAccessPolicies :: Text -> DescribeServiceAccessPolicies
describeServiceAccessPolicies pDomainName =
    DescribeServiceAccessPolicies'
    { _dsaprqDeployed = Nothing
    , _dsaprqDomainName = pDomainName
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
dsaprqDeployed :: Lens' DescribeServiceAccessPolicies (Maybe Bool)
dsaprqDeployed = lens _dsaprqDeployed (\ s a -> s{_dsaprqDeployed = a});

-- | The name of the domain you want to describe.
dsaprqDomainName :: Lens' DescribeServiceAccessPolicies Text
dsaprqDomainName = lens _dsaprqDomainName (\ s a -> s{_dsaprqDomainName = a});

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
                   (pure (fromEnum s)) <*> (x .@ "AccessPolicies"))

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
               "Deployed" =: _dsaprqDeployed,
               "DomainName" =: _dsaprqDomainName]

-- | The result of a @DescribeServiceAccessPolicies@ request.
--
-- /See:/ 'describeServiceAccessPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsaprsStatus'
--
-- * 'dsaprsAccessPolicies'
data DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse'
    { _dsaprsStatus         :: !Int
    , _dsaprsAccessPolicies :: !AccessPoliciesStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServiceAccessPoliciesResponse' smart constructor.
describeServiceAccessPoliciesResponse :: Int -> AccessPoliciesStatus -> DescribeServiceAccessPoliciesResponse
describeServiceAccessPoliciesResponse pStatus pAccessPolicies =
    DescribeServiceAccessPoliciesResponse'
    { _dsaprsStatus = pStatus
    , _dsaprsAccessPolicies = pAccessPolicies
    }

-- | FIXME: Undocumented member.
dsaprsStatus :: Lens' DescribeServiceAccessPoliciesResponse Int
dsaprsStatus = lens _dsaprsStatus (\ s a -> s{_dsaprsStatus = a});

-- | The access rules configured for the domain specified in the request.
dsaprsAccessPolicies :: Lens' DescribeServiceAccessPoliciesResponse AccessPoliciesStatus
dsaprsAccessPolicies = lens _dsaprsAccessPolicies (\ s a -> s{_dsaprsAccessPolicies = a});

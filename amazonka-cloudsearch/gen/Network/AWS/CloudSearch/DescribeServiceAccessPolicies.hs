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
    , dsapDeployed
    , dsapDomainName

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
-- * 'dsapDeployed'
--
-- * 'dsapDomainName'
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies'
    { _dsapDeployed   :: !(Maybe Bool)
    , _dsapDomainName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeServiceAccessPolicies' smart constructor.
describeServiceAccessPolicies :: Text -> DescribeServiceAccessPolicies
describeServiceAccessPolicies pDomainName_ =
    DescribeServiceAccessPolicies'
    { _dsapDeployed = Nothing
    , _dsapDomainName = pDomainName_
    }

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
               "Deployed" =: _dsapDeployed,
               "DomainName" =: _dsapDomainName]

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
describeServiceAccessPoliciesResponse pStatus_ pAccessPolicies_ =
    DescribeServiceAccessPoliciesResponse'
    { _dsaprsStatus = pStatus_
    , _dsaprsAccessPolicies = pAccessPolicies_
    }

-- | FIXME: Undocumented member.
dsaprsStatus :: Lens' DescribeServiceAccessPoliciesResponse Int
dsaprsStatus = lens _dsaprsStatus (\ s a -> s{_dsaprsStatus = a});

-- | The access rules configured for the domain specified in the request.
dsaprsAccessPolicies :: Lens' DescribeServiceAccessPoliciesResponse AccessPoliciesStatus
dsaprsAccessPolicies = lens _dsaprsAccessPolicies (\ s a -> s{_dsaprsAccessPolicies = a});

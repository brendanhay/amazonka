{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.UpdateServiceAccessPolicies
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

-- | Configures the access rules that control access to the domain\'s
-- document and search endpoints. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for an Amazon CloudSearch Domain>.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UpdateServiceAccessPolicies.html>
module Network.AWS.CloudSearch.UpdateServiceAccessPolicies
    (
    -- * Request
      UpdateServiceAccessPolicies
    -- ** Request constructor
    , updateServiceAccessPolicies
    -- ** Request lenses
    , usapDomainName
    , usapAccessPolicies

    -- * Response
    , UpdateServiceAccessPoliciesResponse
    -- ** Response constructor
    , updateServiceAccessPoliciesResponse
    -- ** Response lenses
    , usaprStatus
    , usaprAccessPolicies
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @UpdateServiceAccessPolicies@
-- operation. Specifies the name of the domain you want to update and the
-- access rules you want to configure.
--
-- /See:/ 'updateServiceAccessPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usapDomainName'
--
-- * 'usapAccessPolicies'
data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies'
    { _usapDomainName     :: !Text
    , _usapAccessPolicies :: !Text
    } deriving (Eq,Read,Show)

-- | 'UpdateServiceAccessPolicies' smart constructor.
updateServiceAccessPolicies :: Text -> Text -> UpdateServiceAccessPolicies
updateServiceAccessPolicies pDomainName pAccessPolicies =
    UpdateServiceAccessPolicies'
    { _usapDomainName = pDomainName
    , _usapAccessPolicies = pAccessPolicies
    }

-- | FIXME: Undocumented member.
usapDomainName :: Lens' UpdateServiceAccessPolicies Text
usapDomainName = lens _usapDomainName (\ s a -> s{_usapDomainName = a});

-- | The access rules you want to configure. These rules replace any existing
-- rules.
usapAccessPolicies :: Lens' UpdateServiceAccessPolicies Text
usapAccessPolicies = lens _usapAccessPolicies (\ s a -> s{_usapAccessPolicies = a});

instance AWSRequest UpdateServiceAccessPolicies where
        type Sv UpdateServiceAccessPolicies = CloudSearch
        type Rs UpdateServiceAccessPolicies =
             UpdateServiceAccessPoliciesResponse
        request = post
        response
          = receiveXMLWrapper
              "UpdateServiceAccessPoliciesResult"
              (\ s h x ->
                 UpdateServiceAccessPoliciesResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "AccessPolicies"))

instance ToHeaders UpdateServiceAccessPolicies where
        toHeaders = const mempty

instance ToPath UpdateServiceAccessPolicies where
        toPath = const "/"

instance ToQuery UpdateServiceAccessPolicies where
        toQuery UpdateServiceAccessPolicies'{..}
          = mconcat
              ["Action" =:
                 ("UpdateServiceAccessPolicies" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _usapDomainName,
               "AccessPolicies" =: _usapAccessPolicies]

-- | The result of an @UpdateServiceAccessPolicies@ request. Contains the new
-- access policies.
--
-- /See:/ 'updateServiceAccessPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usaprStatus'
--
-- * 'usaprAccessPolicies'
data UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse'
    { _usaprStatus         :: !Int
    , _usaprAccessPolicies :: !AccessPoliciesStatus
    } deriving (Eq,Read,Show)

-- | 'UpdateServiceAccessPoliciesResponse' smart constructor.
updateServiceAccessPoliciesResponse :: Int -> AccessPoliciesStatus -> UpdateServiceAccessPoliciesResponse
updateServiceAccessPoliciesResponse pStatus pAccessPolicies =
    UpdateServiceAccessPoliciesResponse'
    { _usaprStatus = pStatus
    , _usaprAccessPolicies = pAccessPolicies
    }

-- | FIXME: Undocumented member.
usaprStatus :: Lens' UpdateServiceAccessPoliciesResponse Int
usaprStatus = lens _usaprStatus (\ s a -> s{_usaprStatus = a});

-- | The access rules configured for the domain.
usaprAccessPolicies :: Lens' UpdateServiceAccessPoliciesResponse AccessPoliciesStatus
usaprAccessPolicies = lens _usaprAccessPolicies (\ s a -> s{_usaprAccessPolicies = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateServiceAccessPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Configures the access rules that control access to the domain\'s
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
    , usaprqDomainName
    , usaprqAccessPolicies

    -- * Response
    , UpdateServiceAccessPoliciesResponse
    -- ** Response constructor
    , updateServiceAccessPoliciesResponse
    -- ** Response lenses
    , usaprsStatus
    , usaprsAccessPolicies
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
-- * 'usaprqDomainName'
--
-- * 'usaprqAccessPolicies'
data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies'
    { _usaprqDomainName     :: !Text
    , _usaprqAccessPolicies :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateServiceAccessPolicies' smart constructor.
updateServiceAccessPolicies :: Text -> Text -> UpdateServiceAccessPolicies
updateServiceAccessPolicies pDomainName_ pAccessPolicies_ =
    UpdateServiceAccessPolicies'
    { _usaprqDomainName = pDomainName_
    , _usaprqAccessPolicies = pAccessPolicies_
    }

-- | FIXME: Undocumented member.
usaprqDomainName :: Lens' UpdateServiceAccessPolicies Text
usaprqDomainName = lens _usaprqDomainName (\ s a -> s{_usaprqDomainName = a});

-- | The access rules you want to configure. These rules replace any existing
-- rules.
usaprqAccessPolicies :: Lens' UpdateServiceAccessPolicies Text
usaprqAccessPolicies = lens _usaprqAccessPolicies (\ s a -> s{_usaprqAccessPolicies = a});

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
               "DomainName" =: _usaprqDomainName,
               "AccessPolicies" =: _usaprqAccessPolicies]

-- | The result of an @UpdateServiceAccessPolicies@ request. Contains the new
-- access policies.
--
-- /See:/ 'updateServiceAccessPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usaprsStatus'
--
-- * 'usaprsAccessPolicies'
data UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse'
    { _usaprsStatus         :: !Int
    , _usaprsAccessPolicies :: !AccessPoliciesStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateServiceAccessPoliciesResponse' smart constructor.
updateServiceAccessPoliciesResponse :: Int -> AccessPoliciesStatus -> UpdateServiceAccessPoliciesResponse
updateServiceAccessPoliciesResponse pStatus_ pAccessPolicies_ =
    UpdateServiceAccessPoliciesResponse'
    { _usaprsStatus = pStatus_
    , _usaprsAccessPolicies = pAccessPolicies_
    }

-- | FIXME: Undocumented member.
usaprsStatus :: Lens' UpdateServiceAccessPoliciesResponse Int
usaprsStatus = lens _usaprsStatus (\ s a -> s{_usaprsStatus = a});

-- | The access rules configured for the domain.
usaprsAccessPolicies :: Lens' UpdateServiceAccessPoliciesResponse AccessPoliciesStatus
usaprsAccessPolicies = lens _usaprsAccessPolicies (\ s a -> s{_usaprsAccessPolicies = a});

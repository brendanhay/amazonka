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
-- Module      : Network.AWS.CloudSearch.UpdateServiceAccessPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the access rules that control access to the domain\'s
-- document and search endpoints. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for an Amazon CloudSearch Domain>.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UpdateServiceAccessPolicies.html AWS API Reference> for UpdateServiceAccessPolicies.
module Network.AWS.CloudSearch.UpdateServiceAccessPolicies
    (
    -- * Creating a Request
      updateServiceAccessPolicies
    , UpdateServiceAccessPolicies
    -- * Request Lenses
    , usapDomainName
    , usapAccessPolicies

    -- * Destructuring the Response
    , updateServiceAccessPoliciesResponse
    , UpdateServiceAccessPoliciesResponse
    -- * Response Lenses
    , usaprsStatus
    , usaprsAccessPolicies
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.CloudSearch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the 'UpdateServiceAccessPolicies'
-- operation. Specifies the name of the domain you want to update and the
-- access rules you want to configure.
--
-- /See:/ 'updateServiceAccessPolicies' smart constructor.
data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies'
    { _usapDomainName     :: !Text
    , _usapAccessPolicies :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateServiceAccessPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usapDomainName'
--
-- * 'usapAccessPolicies'
updateServiceAccessPolicies
    :: Text -- ^ 'usapDomainName'
    -> Text -- ^ 'usapAccessPolicies'
    -> UpdateServiceAccessPolicies
updateServiceAccessPolicies pDomainName_ pAccessPolicies_ =
    UpdateServiceAccessPolicies'
    { _usapDomainName = pDomainName_
    , _usapAccessPolicies = pAccessPolicies_
    }

-- | Undocumented member.
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
        request = postQuery
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

-- | The result of an 'UpdateServiceAccessPolicies' request. Contains the new
-- access policies.
--
-- /See:/ 'updateServiceAccessPoliciesResponse' smart constructor.
data UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse'
    { _usaprsStatus         :: !Int
    , _usaprsAccessPolicies :: !AccessPoliciesStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateServiceAccessPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usaprsStatus'
--
-- * 'usaprsAccessPolicies'
updateServiceAccessPoliciesResponse
    :: Int -- ^ 'usaprsStatus'
    -> AccessPoliciesStatus -- ^ 'usaprsAccessPolicies'
    -> UpdateServiceAccessPoliciesResponse
updateServiceAccessPoliciesResponse pStatus_ pAccessPolicies_ =
    UpdateServiceAccessPoliciesResponse'
    { _usaprsStatus = pStatus_
    , _usaprsAccessPolicies = pAccessPolicies_
    }

-- | The response status code.
usaprsStatus :: Lens' UpdateServiceAccessPoliciesResponse Int
usaprsStatus = lens _usaprsStatus (\ s a -> s{_usaprsStatus = a});

-- | The access rules configured for the domain.
usaprsAccessPolicies :: Lens' UpdateServiceAccessPoliciesResponse AccessPoliciesStatus
usaprsAccessPolicies = lens _usaprsAccessPolicies (\ s a -> s{_usaprsAccessPolicies = a});

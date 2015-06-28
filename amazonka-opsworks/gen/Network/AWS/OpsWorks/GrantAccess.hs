{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.GrantAccess
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

-- | This action can be used only with Windows stacks.
--
-- Grants RDP access to a Windows instance for a specified time period.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_GrantAccess.html>
module Network.AWS.OpsWorks.GrantAccess
    (
    -- * Request
      GrantAccess
    -- ** Request constructor
    , grantAccess
    -- ** Request lenses
    , gaValidForInMinutes
    , gaInstanceId

    -- * Response
    , GrantAccessResponse
    -- ** Response constructor
    , grantAccessResponse
    -- ** Response lenses
    , garTemporaryCredential
    , garStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'grantAccess' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaValidForInMinutes'
--
-- * 'gaInstanceId'
data GrantAccess = GrantAccess'
    { _gaValidForInMinutes :: !(Maybe Nat)
    , _gaInstanceId        :: !Text
    } deriving (Eq,Read,Show)

-- | 'GrantAccess' smart constructor.
grantAccess :: Text -> GrantAccess
grantAccess pInstanceId =
    GrantAccess'
    { _gaValidForInMinutes = Nothing
    , _gaInstanceId = pInstanceId
    }

-- | The length of time (in minutes) that the grant is valid. When the grant
-- expires at the end of this period, the user will no longer be able to
-- use the credentials to log in. If the user is logged in at the time, he
-- or she automatically will be logged out.
gaValidForInMinutes :: Lens' GrantAccess (Maybe Natural)
gaValidForInMinutes = lens _gaValidForInMinutes (\ s a -> s{_gaValidForInMinutes = a}) . mapping _Nat;

-- | The instance\'s AWS OpsWorks ID.
gaInstanceId :: Lens' GrantAccess Text
gaInstanceId = lens _gaInstanceId (\ s a -> s{_gaInstanceId = a});

instance AWSRequest GrantAccess where
        type Sv GrantAccess = OpsWorks
        type Rs GrantAccess = GrantAccessResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GrantAccessResponse' <$>
                   (x .?> "TemporaryCredential") <*> (pure s))

instance ToHeaders GrantAccess where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.GrantAccess" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GrantAccess where
        toJSON GrantAccess'{..}
          = object
              ["ValidForInMinutes" .= _gaValidForInMinutes,
               "InstanceId" .= _gaInstanceId]

instance ToPath GrantAccess where
        toPath = const "/"

instance ToQuery GrantAccess where
        toQuery = const mempty

-- | Contains the response to a @GrantAccess@ request.
--
-- /See:/ 'grantAccessResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garTemporaryCredential'
--
-- * 'garStatus'
data GrantAccessResponse = GrantAccessResponse'
    { _garTemporaryCredential :: !(Maybe TemporaryCredential)
    , _garStatus              :: !Status
    } deriving (Eq,Read,Show)

-- | 'GrantAccessResponse' smart constructor.
grantAccessResponse :: Status -> GrantAccessResponse
grantAccessResponse pStatus =
    GrantAccessResponse'
    { _garTemporaryCredential = Nothing
    , _garStatus = pStatus
    }

-- | A @TemporaryCredential@ object that contains the data needed to log in
-- to the instance by RDP clients, such as the Microsoft Remote Desktop
-- Connection.
garTemporaryCredential :: Lens' GrantAccessResponse (Maybe TemporaryCredential)
garTemporaryCredential = lens _garTemporaryCredential (\ s a -> s{_garTemporaryCredential = a});

-- | FIXME: Undocumented member.
garStatus :: Lens' GrantAccessResponse Status
garStatus = lens _garStatus (\ s a -> s{_garStatus = a});

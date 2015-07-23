{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.GrantAccess
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action can be used only with Windows stacks.
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
    , garqValidForInMinutes
    , garqInstanceId

    -- * Response
    , GrantAccessResponse
    -- ** Response constructor
    , grantAccessResponse
    -- ** Response lenses
    , garsTemporaryCredential
    , garsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'grantAccess' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garqValidForInMinutes'
--
-- * 'garqInstanceId'
data GrantAccess = GrantAccess'
    { _garqValidForInMinutes :: !(Maybe Nat)
    , _garqInstanceId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GrantAccess' smart constructor.
grantAccess :: Text -> GrantAccess
grantAccess pInstanceId_ =
    GrantAccess'
    { _garqValidForInMinutes = Nothing
    , _garqInstanceId = pInstanceId_
    }

-- | The length of time (in minutes) that the grant is valid. When the grant
-- expires at the end of this period, the user will no longer be able to
-- use the credentials to log in. If the user is logged in at the time, he
-- or she automatically will be logged out.
garqValidForInMinutes :: Lens' GrantAccess (Maybe Natural)
garqValidForInMinutes = lens _garqValidForInMinutes (\ s a -> s{_garqValidForInMinutes = a}) . mapping _Nat;

-- | The instance\'s AWS OpsWorks ID.
garqInstanceId :: Lens' GrantAccess Text
garqInstanceId = lens _garqInstanceId (\ s a -> s{_garqInstanceId = a});

instance AWSRequest GrantAccess where
        type Sv GrantAccess = OpsWorks
        type Rs GrantAccess = GrantAccessResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GrantAccessResponse' <$>
                   (x .?> "TemporaryCredential") <*>
                     (pure (fromEnum s)))

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
              ["ValidForInMinutes" .= _garqValidForInMinutes,
               "InstanceId" .= _garqInstanceId]

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
-- * 'garsTemporaryCredential'
--
-- * 'garsStatus'
data GrantAccessResponse = GrantAccessResponse'
    { _garsTemporaryCredential :: !(Maybe TemporaryCredential)
    , _garsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GrantAccessResponse' smart constructor.
grantAccessResponse :: Int -> GrantAccessResponse
grantAccessResponse pStatus_ =
    GrantAccessResponse'
    { _garsTemporaryCredential = Nothing
    , _garsStatus = pStatus_
    }

-- | A @TemporaryCredential@ object that contains the data needed to log in
-- to the instance by RDP clients, such as the Microsoft Remote Desktop
-- Connection.
garsTemporaryCredential :: Lens' GrantAccessResponse (Maybe TemporaryCredential)
garsTemporaryCredential = lens _garsTemporaryCredential (\ s a -> s{_garsTemporaryCredential = a});

-- | FIXME: Undocumented member.
garsStatus :: Lens' GrantAccessResponse Int
garsStatus = lens _garsStatus (\ s a -> s{_garsStatus = a});

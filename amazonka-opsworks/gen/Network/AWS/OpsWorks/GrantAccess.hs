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
-- Module      : Network.AWS.OpsWorks.GrantAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants RDP access to a Windows instance for a specified time period.
--
--
module Network.AWS.OpsWorks.GrantAccess
    (
    -- * Creating a Request
      grantAccess
    , GrantAccess
    -- * Request Lenses
    , gaValidForInMinutes
    , gaInstanceId

    -- * Destructuring the Response
    , grantAccessResponse
    , GrantAccessResponse
    -- * Response Lenses
    , garsTemporaryCredential
    , garsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'grantAccess' smart constructor.
data GrantAccess = GrantAccess'
  { _gaValidForInMinutes :: !(Maybe Nat)
  , _gaInstanceId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GrantAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaValidForInMinutes' - The length of time (in minutes) that the grant is valid. When the grant expires at the end of this period, the user will no longer be able to use the credentials to log in. If the user is logged in at the time, he or she automatically will be logged out.
--
-- * 'gaInstanceId' - The instance's AWS OpsWorks Stacks ID.
grantAccess
    :: Text -- ^ 'gaInstanceId'
    -> GrantAccess
grantAccess pInstanceId_ =
  GrantAccess' {_gaValidForInMinutes = Nothing, _gaInstanceId = pInstanceId_}


-- | The length of time (in minutes) that the grant is valid. When the grant expires at the end of this period, the user will no longer be able to use the credentials to log in. If the user is logged in at the time, he or she automatically will be logged out.
gaValidForInMinutes :: Lens' GrantAccess (Maybe Natural)
gaValidForInMinutes = lens _gaValidForInMinutes (\ s a -> s{_gaValidForInMinutes = a}) . mapping _Nat

-- | The instance's AWS OpsWorks Stacks ID.
gaInstanceId :: Lens' GrantAccess Text
gaInstanceId = lens _gaInstanceId (\ s a -> s{_gaInstanceId = a})

instance AWSRequest GrantAccess where
        type Rs GrantAccess = GrantAccessResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 GrantAccessResponse' <$>
                   (x .?> "TemporaryCredential") <*>
                     (pure (fromEnum s)))

instance Hashable GrantAccess where

instance NFData GrantAccess where

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
              (catMaybes
                 [("ValidForInMinutes" .=) <$> _gaValidForInMinutes,
                  Just ("InstanceId" .= _gaInstanceId)])

instance ToPath GrantAccess where
        toPath = const "/"

instance ToQuery GrantAccess where
        toQuery = const mempty

-- | Contains the response to a @GrantAccess@ request.
--
--
--
-- /See:/ 'grantAccessResponse' smart constructor.
data GrantAccessResponse = GrantAccessResponse'
  { _garsTemporaryCredential :: !(Maybe TemporaryCredential)
  , _garsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GrantAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsTemporaryCredential' - A @TemporaryCredential@ object that contains the data needed to log in to the instance by RDP clients, such as the Microsoft Remote Desktop Connection.
--
-- * 'garsResponseStatus' - -- | The response status code.
grantAccessResponse
    :: Int -- ^ 'garsResponseStatus'
    -> GrantAccessResponse
grantAccessResponse pResponseStatus_ =
  GrantAccessResponse'
    {_garsTemporaryCredential = Nothing, _garsResponseStatus = pResponseStatus_}


-- | A @TemporaryCredential@ object that contains the data needed to log in to the instance by RDP clients, such as the Microsoft Remote Desktop Connection.
garsTemporaryCredential :: Lens' GrantAccessResponse (Maybe TemporaryCredential)
garsTemporaryCredential = lens _garsTemporaryCredential (\ s a -> s{_garsTemporaryCredential = a})

-- | -- | The response status code.
garsResponseStatus :: Lens' GrantAccessResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a})

instance NFData GrantAccessResponse where

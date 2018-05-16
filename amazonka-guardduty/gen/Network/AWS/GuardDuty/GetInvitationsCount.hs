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
-- Module      : Network.AWS.GuardDuty.GetInvitationsCount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count of all GuardDuty membership invitations that were sent to the current member account except the currently accepted invitation.
module Network.AWS.GuardDuty.GetInvitationsCount
    (
    -- * Creating a Request
      getInvitationsCount
    , GetInvitationsCount

    -- * Destructuring the Response
    , getInvitationsCountResponse
    , GetInvitationsCountResponse
    -- * Response Lenses
    , gicrsInvitationsCount
    , gicrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInvitationsCount' smart constructor.
data GetInvitationsCount =
  GetInvitationsCount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInvitationsCount' with the minimum fields required to make a request.
--
getInvitationsCount
    :: GetInvitationsCount
getInvitationsCount = GetInvitationsCount'


instance AWSRequest GetInvitationsCount where
        type Rs GetInvitationsCount =
             GetInvitationsCountResponse
        request = get guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 GetInvitationsCountResponse' <$>
                   (x .?> "invitationsCount") <*> (pure (fromEnum s)))

instance Hashable GetInvitationsCount where

instance NFData GetInvitationsCount where

instance ToHeaders GetInvitationsCount where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetInvitationsCount where
        toPath = const "/invitation/count"

instance ToQuery GetInvitationsCount where
        toQuery = const mempty

-- | /See:/ 'getInvitationsCountResponse' smart constructor.
data GetInvitationsCountResponse = GetInvitationsCountResponse'
  { _gicrsInvitationsCount :: !(Maybe Int)
  , _gicrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInvitationsCountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gicrsInvitationsCount' - The number of received invitations.
--
-- * 'gicrsResponseStatus' - -- | The response status code.
getInvitationsCountResponse
    :: Int -- ^ 'gicrsResponseStatus'
    -> GetInvitationsCountResponse
getInvitationsCountResponse pResponseStatus_ =
  GetInvitationsCountResponse'
    {_gicrsInvitationsCount = Nothing, _gicrsResponseStatus = pResponseStatus_}


-- | The number of received invitations.
gicrsInvitationsCount :: Lens' GetInvitationsCountResponse (Maybe Int)
gicrsInvitationsCount = lens _gicrsInvitationsCount (\ s a -> s{_gicrsInvitationsCount = a})

-- | -- | The response status code.
gicrsResponseStatus :: Lens' GetInvitationsCountResponse Int
gicrsResponseStatus = lens _gicrsResponseStatus (\ s a -> s{_gicrsResponseStatus = a})

instance NFData GetInvitationsCountResponse where

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
-- Module      : Network.AWS.WorkDocs.DeactivateUser
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified user, which revokes the user's access to Amazon WorkDocs.
--
--
module Network.AWS.WorkDocs.DeactivateUser
    (
    -- * Creating a Request
      deactivateUser
    , DeactivateUser
    -- * Request Lenses
    , duUserId

    -- * Destructuring the Response
    , deactivateUserResponse
    , DeactivateUserResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deactivateUser' smart constructor.
newtype DeactivateUser = DeactivateUser'
    { _duUserId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeactivateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duUserId' - The ID of the user.
deactivateUser
    :: Text -- ^ 'duUserId'
    -> DeactivateUser
deactivateUser pUserId_ =
    DeactivateUser'
    { _duUserId = pUserId_
    }

-- | The ID of the user.
duUserId :: Lens' DeactivateUser Text
duUserId = lens _duUserId (\ s a -> s{_duUserId = a});

instance AWSRequest DeactivateUser where
        type Rs DeactivateUser = DeactivateUserResponse
        request = delete workDocs
        response = receiveNull DeactivateUserResponse'

instance Hashable DeactivateUser

instance NFData DeactivateUser

instance ToHeaders DeactivateUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeactivateUser where
        toPath DeactivateUser'{..}
          = mconcat
              ["/api/v1/users/", toBS _duUserId, "/activation"]

instance ToQuery DeactivateUser where
        toQuery = const mempty

-- | /See:/ 'deactivateUserResponse' smart constructor.
data DeactivateUserResponse =
    DeactivateUserResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeactivateUserResponse' with the minimum fields required to make a request.
--
deactivateUserResponse
    :: DeactivateUserResponse
deactivateUserResponse = DeactivateUserResponse'

instance NFData DeactivateUserResponse

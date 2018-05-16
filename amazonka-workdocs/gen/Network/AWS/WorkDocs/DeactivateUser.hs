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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    , dAuthenticationToken
    , dUserId

    -- * Destructuring the Response
    , deactivateUserResponse
    , DeactivateUserResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deactivateUser' smart constructor.
data DeactivateUser = DeactivateUser'
  { _dAuthenticationToken :: !(Maybe (Sensitive Text))
  , _dUserId              :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeactivateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'dUserId' - The ID of the user.
deactivateUser
    :: Text -- ^ 'dUserId'
    -> DeactivateUser
deactivateUser pUserId_ =
  DeactivateUser' {_dAuthenticationToken = Nothing, _dUserId = pUserId_}


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
dAuthenticationToken :: Lens' DeactivateUser (Maybe Text)
dAuthenticationToken = lens _dAuthenticationToken (\ s a -> s{_dAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the user.
dUserId :: Lens' DeactivateUser Text
dUserId = lens _dUserId (\ s a -> s{_dUserId = a})

instance AWSRequest DeactivateUser where
        type Rs DeactivateUser = DeactivateUserResponse
        request = delete workDocs
        response = receiveNull DeactivateUserResponse'

instance Hashable DeactivateUser where

instance NFData DeactivateUser where

instance ToHeaders DeactivateUser where
        toHeaders DeactivateUser'{..}
          = mconcat
              ["Authentication" =# _dAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DeactivateUser where
        toPath DeactivateUser'{..}
          = mconcat
              ["/api/v1/users/", toBS _dUserId, "/activation"]

instance ToQuery DeactivateUser where
        toQuery = const mempty

-- | /See:/ 'deactivateUserResponse' smart constructor.
data DeactivateUserResponse =
  DeactivateUserResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeactivateUserResponse' with the minimum fields required to make a request.
--
deactivateUserResponse
    :: DeactivateUserResponse
deactivateUserResponse = DeactivateUserResponse'


instance NFData DeactivateUserResponse where

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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Cognito user pool.
--
--
module Network.AWS.CognitoIdentityProvider.DeleteUserPool
    (
    -- * Creating a Request
      deleteUserPool
    , DeleteUserPool
    -- * Request Lenses
    , dupUserPoolId

    -- * Destructuring the Response
    , deleteUserPoolResponse
    , DeleteUserPoolResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to delete a user pool.
--
--
--
-- /See:/ 'deleteUserPool' smart constructor.
newtype DeleteUserPool = DeleteUserPool'
  { _dupUserPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupUserPoolId' - The user pool ID for the user pool you want to delete.
deleteUserPool
    :: Text -- ^ 'dupUserPoolId'
    -> DeleteUserPool
deleteUserPool pUserPoolId_ = DeleteUserPool' {_dupUserPoolId = pUserPoolId_}


-- | The user pool ID for the user pool you want to delete.
dupUserPoolId :: Lens' DeleteUserPool Text
dupUserPoolId = lens _dupUserPoolId (\ s a -> s{_dupUserPoolId = a})

instance AWSRequest DeleteUserPool where
        type Rs DeleteUserPool = DeleteUserPoolResponse
        request = postJSON cognitoIdentityProvider
        response = receiveNull DeleteUserPoolResponse'

instance Hashable DeleteUserPool where

instance NFData DeleteUserPool where

instance ToHeaders DeleteUserPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DeleteUserPool"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUserPool where
        toJSON DeleteUserPool'{..}
          = object
              (catMaybes [Just ("UserPoolId" .= _dupUserPoolId)])

instance ToPath DeleteUserPool where
        toPath = const "/"

instance ToQuery DeleteUserPool where
        toQuery = const mempty

-- | /See:/ 'deleteUserPoolResponse' smart constructor.
data DeleteUserPoolResponse =
  DeleteUserPoolResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPoolResponse' with the minimum fields required to make a request.
--
deleteUserPoolResponse
    :: DeleteUserPoolResponse
deleteUserPoolResponse = DeleteUserPoolResponse'


instance NFData DeleteUserPoolResponse where

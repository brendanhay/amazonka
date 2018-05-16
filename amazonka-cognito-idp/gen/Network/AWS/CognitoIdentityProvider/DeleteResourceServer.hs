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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteResourceServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource server.
--
--
module Network.AWS.CognitoIdentityProvider.DeleteResourceServer
    (
    -- * Creating a Request
      deleteResourceServer
    , DeleteResourceServer
    -- * Request Lenses
    , drsUserPoolId
    , drsIdentifier

    -- * Destructuring the Response
    , deleteResourceServerResponse
    , DeleteResourceServerResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteResourceServer' smart constructor.
data DeleteResourceServer = DeleteResourceServer'
  { _drsUserPoolId :: !Text
  , _drsIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResourceServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsUserPoolId' - The user pool ID for the user pool that hosts the resource server.
--
-- * 'drsIdentifier' - The identifier for the resource server.
deleteResourceServer
    :: Text -- ^ 'drsUserPoolId'
    -> Text -- ^ 'drsIdentifier'
    -> DeleteResourceServer
deleteResourceServer pUserPoolId_ pIdentifier_ =
  DeleteResourceServer'
    {_drsUserPoolId = pUserPoolId_, _drsIdentifier = pIdentifier_}


-- | The user pool ID for the user pool that hosts the resource server.
drsUserPoolId :: Lens' DeleteResourceServer Text
drsUserPoolId = lens _drsUserPoolId (\ s a -> s{_drsUserPoolId = a})

-- | The identifier for the resource server.
drsIdentifier :: Lens' DeleteResourceServer Text
drsIdentifier = lens _drsIdentifier (\ s a -> s{_drsIdentifier = a})

instance AWSRequest DeleteResourceServer where
        type Rs DeleteResourceServer =
             DeleteResourceServerResponse
        request = postJSON cognitoIdentityProvider
        response = receiveNull DeleteResourceServerResponse'

instance Hashable DeleteResourceServer where

instance NFData DeleteResourceServer where

instance ToHeaders DeleteResourceServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DeleteResourceServer"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteResourceServer where
        toJSON DeleteResourceServer'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _drsUserPoolId),
                  Just ("Identifier" .= _drsIdentifier)])

instance ToPath DeleteResourceServer where
        toPath = const "/"

instance ToQuery DeleteResourceServer where
        toQuery = const mempty

-- | /See:/ 'deleteResourceServerResponse' smart constructor.
data DeleteResourceServerResponse =
  DeleteResourceServerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResourceServerResponse' with the minimum fields required to make a request.
--
deleteResourceServerResponse
    :: DeleteResourceServerResponse
deleteResourceServerResponse = DeleteResourceServerResponse'


instance NFData DeleteResourceServerResponse where

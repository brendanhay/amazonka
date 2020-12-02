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
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentityPool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user pool. Once a pool is deleted, users will not be able to authenticate with the pool.
--
--
-- You must use AWS Developer credentials to call this API.
--
module Network.AWS.CognitoIdentity.DeleteIdentityPool
    (
    -- * Creating a Request
      deleteIdentityPool
    , DeleteIdentityPool
    -- * Request Lenses
    , dIdentityPoolId

    -- * Destructuring the Response
    , deleteIdentityPoolResponse
    , DeleteIdentityPoolResponse
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the DeleteIdentityPool action.
--
--
--
-- /See:/ 'deleteIdentityPool' smart constructor.
newtype DeleteIdentityPool = DeleteIdentityPool'
  { _dIdentityPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIdentityPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dIdentityPoolId' - An identity pool ID in the format REGION:GUID.
deleteIdentityPool
    :: Text -- ^ 'dIdentityPoolId'
    -> DeleteIdentityPool
deleteIdentityPool pIdentityPoolId_ =
  DeleteIdentityPool' {_dIdentityPoolId = pIdentityPoolId_}


-- | An identity pool ID in the format REGION:GUID.
dIdentityPoolId :: Lens' DeleteIdentityPool Text
dIdentityPoolId = lens _dIdentityPoolId (\ s a -> s{_dIdentityPoolId = a})

instance AWSRequest DeleteIdentityPool where
        type Rs DeleteIdentityPool =
             DeleteIdentityPoolResponse
        request = postJSON cognitoIdentity
        response = receiveNull DeleteIdentityPoolResponse'

instance Hashable DeleteIdentityPool where

instance NFData DeleteIdentityPool where

instance ToHeaders DeleteIdentityPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.DeleteIdentityPool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteIdentityPool where
        toJSON DeleteIdentityPool'{..}
          = object
              (catMaybes
                 [Just ("IdentityPoolId" .= _dIdentityPoolId)])

instance ToPath DeleteIdentityPool where
        toPath = const "/"

instance ToQuery DeleteIdentityPool where
        toQuery = const mempty

-- | /See:/ 'deleteIdentityPoolResponse' smart constructor.
data DeleteIdentityPoolResponse =
  DeleteIdentityPoolResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIdentityPoolResponse' with the minimum fields required to make a request.
--
deleteIdentityPoolResponse
    :: DeleteIdentityPoolResponse
deleteIdentityPoolResponse = DeleteIdentityPoolResponse'


instance NFData DeleteIdentityPoolResponse where

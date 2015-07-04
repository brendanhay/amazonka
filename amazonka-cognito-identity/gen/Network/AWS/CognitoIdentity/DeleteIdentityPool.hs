{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CognitoIdentity.DeleteIdentityPool
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a user pool. Once a pool is deleted, users will not be able to
-- authenticate with the pool.
--
-- You must use AWS Developer credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_DeleteIdentityPool.html>
module Network.AWS.CognitoIdentity.DeleteIdentityPool
    (
    -- * Request
      DeleteIdentityPool
    -- ** Request constructor
    , deleteIdentityPool
    -- ** Request lenses
    , delIdentityPoolId

    -- * Response
    , DeleteIdentityPoolResponse
    -- ** Response constructor
    , deleteIdentityPoolResponse
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the DeleteIdentityPool action.
--
-- /See:/ 'deleteIdentityPool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delIdentityPoolId'
newtype DeleteIdentityPool = DeleteIdentityPool'
    { _delIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentityPool' smart constructor.
deleteIdentityPool :: Text -> DeleteIdentityPool
deleteIdentityPool pIdentityPoolId =
    DeleteIdentityPool'
    { _delIdentityPoolId = pIdentityPoolId
    }

-- | An identity pool ID in the format REGION:GUID.
delIdentityPoolId :: Lens' DeleteIdentityPool Text
delIdentityPoolId = lens _delIdentityPoolId (\ s a -> s{_delIdentityPoolId = a});

instance AWSRequest DeleteIdentityPool where
        type Sv DeleteIdentityPool = CognitoIdentity
        type Rs DeleteIdentityPool =
             DeleteIdentityPoolResponse
        request = postJSON
        response = receiveNull DeleteIdentityPoolResponse'

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
          = object ["IdentityPoolId" .= _delIdentityPoolId]

instance ToPath DeleteIdentityPool where
        toPath = const "/"

instance ToQuery DeleteIdentityPool where
        toQuery = const mempty

-- | /See:/ 'deleteIdentityPoolResponse' smart constructor.
data DeleteIdentityPoolResponse =
    DeleteIdentityPoolResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentityPoolResponse' smart constructor.
deleteIdentityPoolResponse :: DeleteIdentityPoolResponse
deleteIdentityPoolResponse = DeleteIdentityPoolResponse'

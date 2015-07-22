{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentityPool
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user pool. Once a pool is deleted, users will not be able to
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
    , drqIdentityPoolId

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
-- * 'drqIdentityPoolId'
newtype DeleteIdentityPool = DeleteIdentityPool'
    { _drqIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteIdentityPool' smart constructor.
deleteIdentityPool :: Text -> DeleteIdentityPool
deleteIdentityPool pIdentityPoolId =
    DeleteIdentityPool'
    { _drqIdentityPoolId = pIdentityPoolId
    }

-- | An identity pool ID in the format REGION:GUID.
drqIdentityPoolId :: Lens' DeleteIdentityPool Text
drqIdentityPoolId = lens _drqIdentityPoolId (\ s a -> s{_drqIdentityPoolId = a});

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
          = object ["IdentityPoolId" .= _drqIdentityPoolId]

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

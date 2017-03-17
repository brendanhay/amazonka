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
-- Module      : Network.AWS.WorkDocs.RemoveAllResourcePermissions
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes all the permissions from the specified resource.
--
--
module Network.AWS.WorkDocs.RemoveAllResourcePermissions
    (
    -- * Creating a Request
      removeAllResourcePermissions
    , RemoveAllResourcePermissions
    -- * Request Lenses
    , rarpResourceId

    -- * Destructuring the Response
    , removeAllResourcePermissionsResponse
    , RemoveAllResourcePermissionsResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'removeAllResourcePermissions' smart constructor.
newtype RemoveAllResourcePermissions = RemoveAllResourcePermissions'
    { _rarpResourceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveAllResourcePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rarpResourceId' - The ID of the resource.
removeAllResourcePermissions
    :: Text -- ^ 'rarpResourceId'
    -> RemoveAllResourcePermissions
removeAllResourcePermissions pResourceId_ =
    RemoveAllResourcePermissions'
    { _rarpResourceId = pResourceId_
    }

-- | The ID of the resource.
rarpResourceId :: Lens' RemoveAllResourcePermissions Text
rarpResourceId = lens _rarpResourceId (\ s a -> s{_rarpResourceId = a});

instance AWSRequest RemoveAllResourcePermissions
         where
        type Rs RemoveAllResourcePermissions =
             RemoveAllResourcePermissionsResponse
        request = delete workDocs
        response
          = receiveNull RemoveAllResourcePermissionsResponse'

instance Hashable RemoveAllResourcePermissions

instance NFData RemoveAllResourcePermissions

instance ToHeaders RemoveAllResourcePermissions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath RemoveAllResourcePermissions where
        toPath RemoveAllResourcePermissions'{..}
          = mconcat
              ["/api/v1/resources/", toBS _rarpResourceId,
               "/permissions"]

instance ToQuery RemoveAllResourcePermissions where
        toQuery = const mempty

-- | /See:/ 'removeAllResourcePermissionsResponse' smart constructor.
data RemoveAllResourcePermissionsResponse =
    RemoveAllResourcePermissionsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveAllResourcePermissionsResponse' with the minimum fields required to make a request.
--
removeAllResourcePermissionsResponse
    :: RemoveAllResourcePermissionsResponse
removeAllResourcePermissionsResponse = RemoveAllResourcePermissionsResponse'

instance NFData RemoveAllResourcePermissionsResponse

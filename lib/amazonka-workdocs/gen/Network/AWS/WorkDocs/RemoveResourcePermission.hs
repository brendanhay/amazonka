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
-- Module      : Network.AWS.WorkDocs.RemoveResourcePermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the permission for the specified principal from the specified resource.
--
--
module Network.AWS.WorkDocs.RemoveResourcePermission
    (
    -- * Creating a Request
      removeResourcePermission
    , RemoveResourcePermission
    -- * Request Lenses
    , rrpPrincipalType
    , rrpAuthenticationToken
    , rrpResourceId
    , rrpPrincipalId

    -- * Destructuring the Response
    , removeResourcePermissionResponse
    , RemoveResourcePermissionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'removeResourcePermission' smart constructor.
data RemoveResourcePermission = RemoveResourcePermission'
  { _rrpPrincipalType       :: !(Maybe PrincipalType)
  , _rrpAuthenticationToken :: !(Maybe (Sensitive Text))
  , _rrpResourceId          :: !Text
  , _rrpPrincipalId         :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveResourcePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrpPrincipalType' - The principal type of the resource.
--
-- * 'rrpAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'rrpResourceId' - The ID of the resource.
--
-- * 'rrpPrincipalId' - The principal ID of the resource.
removeResourcePermission
    :: Text -- ^ 'rrpResourceId'
    -> Text -- ^ 'rrpPrincipalId'
    -> RemoveResourcePermission
removeResourcePermission pResourceId_ pPrincipalId_ =
  RemoveResourcePermission'
    { _rrpPrincipalType = Nothing
    , _rrpAuthenticationToken = Nothing
    , _rrpResourceId = pResourceId_
    , _rrpPrincipalId = pPrincipalId_
    }


-- | The principal type of the resource.
rrpPrincipalType :: Lens' RemoveResourcePermission (Maybe PrincipalType)
rrpPrincipalType = lens _rrpPrincipalType (\ s a -> s{_rrpPrincipalType = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
rrpAuthenticationToken :: Lens' RemoveResourcePermission (Maybe Text)
rrpAuthenticationToken = lens _rrpAuthenticationToken (\ s a -> s{_rrpAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the resource.
rrpResourceId :: Lens' RemoveResourcePermission Text
rrpResourceId = lens _rrpResourceId (\ s a -> s{_rrpResourceId = a})

-- | The principal ID of the resource.
rrpPrincipalId :: Lens' RemoveResourcePermission Text
rrpPrincipalId = lens _rrpPrincipalId (\ s a -> s{_rrpPrincipalId = a})

instance AWSRequest RemoveResourcePermission where
        type Rs RemoveResourcePermission =
             RemoveResourcePermissionResponse
        request = delete workDocs
        response
          = receiveNull RemoveResourcePermissionResponse'

instance Hashable RemoveResourcePermission where

instance NFData RemoveResourcePermission where

instance ToHeaders RemoveResourcePermission where
        toHeaders RemoveResourcePermission'{..}
          = mconcat
              ["Authentication" =# _rrpAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath RemoveResourcePermission where
        toPath RemoveResourcePermission'{..}
          = mconcat
              ["/api/v1/resources/", toBS _rrpResourceId,
               "/permissions/", toBS _rrpPrincipalId]

instance ToQuery RemoveResourcePermission where
        toQuery RemoveResourcePermission'{..}
          = mconcat ["type" =: _rrpPrincipalType]

-- | /See:/ 'removeResourcePermissionResponse' smart constructor.
data RemoveResourcePermissionResponse =
  RemoveResourcePermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveResourcePermissionResponse' with the minimum fields required to make a request.
--
removeResourcePermissionResponse
    :: RemoveResourcePermissionResponse
removeResourcePermissionResponse = RemoveResourcePermissionResponse'


instance NFData RemoveResourcePermissionResponse
         where

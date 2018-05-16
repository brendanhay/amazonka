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
-- Module      : Network.AWS.WorkMail.DeleteResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource.
--
--
module Network.AWS.WorkMail.DeleteResource
    (
    -- * Creating a Request
      deleteResource
    , DeleteResource
    -- * Request Lenses
    , dOrganizationId
    , dResourceId

    -- * Destructuring the Response
    , deleteResourceResponse
    , DeleteResourceResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'deleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { _dOrganizationId :: !Text
  , _dResourceId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dOrganizationId' - The identifier associated with the organization for which the resource is deleted.
--
-- * 'dResourceId' - The identifier of the resource to be deleted.
deleteResource
    :: Text -- ^ 'dOrganizationId'
    -> Text -- ^ 'dResourceId'
    -> DeleteResource
deleteResource pOrganizationId_ pResourceId_ =
  DeleteResource'
    {_dOrganizationId = pOrganizationId_, _dResourceId = pResourceId_}


-- | The identifier associated with the organization for which the resource is deleted.
dOrganizationId :: Lens' DeleteResource Text
dOrganizationId = lens _dOrganizationId (\ s a -> s{_dOrganizationId = a})

-- | The identifier of the resource to be deleted.
dResourceId :: Lens' DeleteResource Text
dResourceId = lens _dResourceId (\ s a -> s{_dResourceId = a})

instance AWSRequest DeleteResource where
        type Rs DeleteResource = DeleteResourceResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteResourceResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteResource where

instance NFData DeleteResource where

instance ToHeaders DeleteResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DeleteResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteResource where
        toJSON DeleteResource'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _dOrganizationId),
                  Just ("ResourceId" .= _dResourceId)])

instance ToPath DeleteResource where
        toPath = const "/"

instance ToQuery DeleteResource where
        toQuery = const mempty

-- | /See:/ 'deleteResourceResponse' smart constructor.
newtype DeleteResourceResponse = DeleteResourceResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteResourceResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteResourceResponse
deleteResourceResponse pResponseStatus_ =
  DeleteResourceResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteResourceResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteResourceResponse where

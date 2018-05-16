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
-- Module      : Network.AWS.IAM.DeleteServiceLinkedRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a service-linked role deletion request and returns a @DeletionTaskId@ , which you can use to check the status of the deletion. Before you call this operation, confirm that the role has no active sessions and that any resources used by the role in the linked service are deleted. If you call this operation more than once for the same service-linked role and an earlier deletion task is not complete, then the @DeletionTaskId@ of the earlier request is returned.
--
--
-- If you submit a deletion request for a service-linked role whose linked service is still accessing a resource, then the deletion task fails. If it fails, the 'GetServiceLinkedRoleDeletionStatus' API operation returns the reason for the failure, usually including the resources that must be deleted. To delete the service-linked role, you must first remove those resources from the linked service and then submit the deletion request again. Resources are specific to the service that is linked to the role. For more information about removing resources from a service, see the <http://docs.aws.amazon.com/ AWS documentation> for your service.
--
-- For more information about service-linked roles, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-service-linked-role Roles Terms and Concepts: AWS Service-Linked Role> in the /IAM User Guide/ .
--
module Network.AWS.IAM.DeleteServiceLinkedRole
    (
    -- * Creating a Request
      deleteServiceLinkedRole
    , DeleteServiceLinkedRole
    -- * Request Lenses
    , dslrRoleName

    -- * Destructuring the Response
    , deleteServiceLinkedRoleResponse
    , DeleteServiceLinkedRoleResponse
    -- * Response Lenses
    , dslrrsResponseStatus
    , dslrrsDeletionTaskId
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteServiceLinkedRole' smart constructor.
newtype DeleteServiceLinkedRole = DeleteServiceLinkedRole'
  { _dslrRoleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceLinkedRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dslrRoleName' - The name of the service-linked role to be deleted.
deleteServiceLinkedRole
    :: Text -- ^ 'dslrRoleName'
    -> DeleteServiceLinkedRole
deleteServiceLinkedRole pRoleName_ =
  DeleteServiceLinkedRole' {_dslrRoleName = pRoleName_}


-- | The name of the service-linked role to be deleted.
dslrRoleName :: Lens' DeleteServiceLinkedRole Text
dslrRoleName = lens _dslrRoleName (\ s a -> s{_dslrRoleName = a})

instance AWSRequest DeleteServiceLinkedRole where
        type Rs DeleteServiceLinkedRole =
             DeleteServiceLinkedRoleResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "DeleteServiceLinkedRoleResult"
              (\ s h x ->
                 DeleteServiceLinkedRoleResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "DeletionTaskId"))

instance Hashable DeleteServiceLinkedRole where

instance NFData DeleteServiceLinkedRole where

instance ToHeaders DeleteServiceLinkedRole where
        toHeaders = const mempty

instance ToPath DeleteServiceLinkedRole where
        toPath = const "/"

instance ToQuery DeleteServiceLinkedRole where
        toQuery DeleteServiceLinkedRole'{..}
          = mconcat
              ["Action" =:
                 ("DeleteServiceLinkedRole" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _dslrRoleName]

-- | /See:/ 'deleteServiceLinkedRoleResponse' smart constructor.
data DeleteServiceLinkedRoleResponse = DeleteServiceLinkedRoleResponse'
  { _dslrrsResponseStatus :: !Int
  , _dslrrsDeletionTaskId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceLinkedRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dslrrsResponseStatus' - -- | The response status code.
--
-- * 'dslrrsDeletionTaskId' - The deletion task identifier that you can use to check the status of the deletion. This identifier is returned in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
deleteServiceLinkedRoleResponse
    :: Int -- ^ 'dslrrsResponseStatus'
    -> Text -- ^ 'dslrrsDeletionTaskId'
    -> DeleteServiceLinkedRoleResponse
deleteServiceLinkedRoleResponse pResponseStatus_ pDeletionTaskId_ =
  DeleteServiceLinkedRoleResponse'
    { _dslrrsResponseStatus = pResponseStatus_
    , _dslrrsDeletionTaskId = pDeletionTaskId_
    }


-- | -- | The response status code.
dslrrsResponseStatus :: Lens' DeleteServiceLinkedRoleResponse Int
dslrrsResponseStatus = lens _dslrrsResponseStatus (\ s a -> s{_dslrrsResponseStatus = a})

-- | The deletion task identifier that you can use to check the status of the deletion. This identifier is returned in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
dslrrsDeletionTaskId :: Lens' DeleteServiceLinkedRoleResponse Text
dslrrsDeletionTaskId = lens _dslrrsDeletionTaskId (\ s a -> s{_dslrrsDeletionTaskId = a})

instance NFData DeleteServiceLinkedRoleResponse where

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
-- Module      : Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of your service-linked role deletion. After you use the 'DeleteServiceLinkedRole' API operation to submit a service-linked role for deletion, you can use the @DeletionTaskId@ parameter in @GetServiceLinkedRoleDeletionStatus@ to check the status of the deletion. If the deletion fails, this operation returns the reason that it failed, if that information is returned by the service.
--
--
module Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
    (
    -- * Creating a Request
      getServiceLinkedRoleDeletionStatus
    , GetServiceLinkedRoleDeletionStatus
    -- * Request Lenses
    , gslrdsDeletionTaskId

    -- * Destructuring the Response
    , getServiceLinkedRoleDeletionStatusResponse
    , GetServiceLinkedRoleDeletionStatusResponse
    -- * Response Lenses
    , gslrdsrsReason
    , gslrdsrsResponseStatus
    , gslrdsrsStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getServiceLinkedRoleDeletionStatus' smart constructor.
newtype GetServiceLinkedRoleDeletionStatus = GetServiceLinkedRoleDeletionStatus'
  { _gslrdsDeletionTaskId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServiceLinkedRoleDeletionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gslrdsDeletionTaskId' - The deletion task identifier. This identifier is returned by the 'DeleteServiceLinkedRole' operation in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
getServiceLinkedRoleDeletionStatus
    :: Text -- ^ 'gslrdsDeletionTaskId'
    -> GetServiceLinkedRoleDeletionStatus
getServiceLinkedRoleDeletionStatus pDeletionTaskId_ =
  GetServiceLinkedRoleDeletionStatus' {_gslrdsDeletionTaskId = pDeletionTaskId_}


-- | The deletion task identifier. This identifier is returned by the 'DeleteServiceLinkedRole' operation in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
gslrdsDeletionTaskId :: Lens' GetServiceLinkedRoleDeletionStatus Text
gslrdsDeletionTaskId = lens _gslrdsDeletionTaskId (\ s a -> s{_gslrdsDeletionTaskId = a})

instance AWSRequest
           GetServiceLinkedRoleDeletionStatus
         where
        type Rs GetServiceLinkedRoleDeletionStatus =
             GetServiceLinkedRoleDeletionStatusResponse
        request = postQuery iam
        response
          = receiveXMLWrapper
              "GetServiceLinkedRoleDeletionStatusResult"
              (\ s h x ->
                 GetServiceLinkedRoleDeletionStatusResponse' <$>
                   (x .@? "Reason") <*> (pure (fromEnum s)) <*>
                     (x .@ "Status"))

instance Hashable GetServiceLinkedRoleDeletionStatus
         where

instance NFData GetServiceLinkedRoleDeletionStatus
         where

instance ToHeaders GetServiceLinkedRoleDeletionStatus
         where
        toHeaders = const mempty

instance ToPath GetServiceLinkedRoleDeletionStatus
         where
        toPath = const "/"

instance ToQuery GetServiceLinkedRoleDeletionStatus
         where
        toQuery GetServiceLinkedRoleDeletionStatus'{..}
          = mconcat
              ["Action" =:
                 ("GetServiceLinkedRoleDeletionStatus" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "DeletionTaskId" =: _gslrdsDeletionTaskId]

-- | /See:/ 'getServiceLinkedRoleDeletionStatusResponse' smart constructor.
data GetServiceLinkedRoleDeletionStatusResponse = GetServiceLinkedRoleDeletionStatusResponse'
  { _gslrdsrsReason         :: !(Maybe DeletionTaskFailureReasonType)
  , _gslrdsrsResponseStatus :: !Int
  , _gslrdsrsStatus         :: !DeletionTaskStatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServiceLinkedRoleDeletionStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gslrdsrsReason' - An object that contains details about the reason the deletion failed.
--
-- * 'gslrdsrsResponseStatus' - -- | The response status code.
--
-- * 'gslrdsrsStatus' - The status of the deletion.
getServiceLinkedRoleDeletionStatusResponse
    :: Int -- ^ 'gslrdsrsResponseStatus'
    -> DeletionTaskStatusType -- ^ 'gslrdsrsStatus'
    -> GetServiceLinkedRoleDeletionStatusResponse
getServiceLinkedRoleDeletionStatusResponse pResponseStatus_ pStatus_ =
  GetServiceLinkedRoleDeletionStatusResponse'
    { _gslrdsrsReason = Nothing
    , _gslrdsrsResponseStatus = pResponseStatus_
    , _gslrdsrsStatus = pStatus_
    }


-- | An object that contains details about the reason the deletion failed.
gslrdsrsReason :: Lens' GetServiceLinkedRoleDeletionStatusResponse (Maybe DeletionTaskFailureReasonType)
gslrdsrsReason = lens _gslrdsrsReason (\ s a -> s{_gslrdsrsReason = a})

-- | -- | The response status code.
gslrdsrsResponseStatus :: Lens' GetServiceLinkedRoleDeletionStatusResponse Int
gslrdsrsResponseStatus = lens _gslrdsrsResponseStatus (\ s a -> s{_gslrdsrsResponseStatus = a})

-- | The status of the deletion.
gslrdsrsStatus :: Lens' GetServiceLinkedRoleDeletionStatusResponse DeletionTaskStatusType
gslrdsrsStatus = lens _gslrdsrsStatus (\ s a -> s{_gslrdsrsStatus = a})

instance NFData
           GetServiceLinkedRoleDeletionStatusResponse
         where

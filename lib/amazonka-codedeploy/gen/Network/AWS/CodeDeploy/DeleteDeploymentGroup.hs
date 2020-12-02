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
-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment group.
--
--
module Network.AWS.CodeDeploy.DeleteDeploymentGroup
    (
    -- * Creating a Request
      deleteDeploymentGroup
    , DeleteDeploymentGroup
    -- * Request Lenses
    , ddgApplicationName
    , ddgDeploymentGroupName

    -- * Destructuring the Response
    , deleteDeploymentGroupResponse
    , DeleteDeploymentGroupResponse
    -- * Response Lenses
    , ddgrsHooksNotCleanedUp
    , ddgrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a DeleteDeploymentGroup operation.
--
--
--
-- /See:/ 'deleteDeploymentGroup' smart constructor.
data DeleteDeploymentGroup = DeleteDeploymentGroup'
  { _ddgApplicationName     :: !Text
  , _ddgDeploymentGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeploymentGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddgApplicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
--
-- * 'ddgDeploymentGroupName' - The name of an existing deployment group for the specified application.
deleteDeploymentGroup
    :: Text -- ^ 'ddgApplicationName'
    -> Text -- ^ 'ddgDeploymentGroupName'
    -> DeleteDeploymentGroup
deleteDeploymentGroup pApplicationName_ pDeploymentGroupName_ =
  DeleteDeploymentGroup'
    { _ddgApplicationName = pApplicationName_
    , _ddgDeploymentGroupName = pDeploymentGroupName_
    }


-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
ddgApplicationName :: Lens' DeleteDeploymentGroup Text
ddgApplicationName = lens _ddgApplicationName (\ s a -> s{_ddgApplicationName = a})

-- | The name of an existing deployment group for the specified application.
ddgDeploymentGroupName :: Lens' DeleteDeploymentGroup Text
ddgDeploymentGroupName = lens _ddgDeploymentGroupName (\ s a -> s{_ddgDeploymentGroupName = a})

instance AWSRequest DeleteDeploymentGroup where
        type Rs DeleteDeploymentGroup =
             DeleteDeploymentGroupResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDeploymentGroupResponse' <$>
                   (x .?> "hooksNotCleanedUp" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteDeploymentGroup where

instance NFData DeleteDeploymentGroup where

instance ToHeaders DeleteDeploymentGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.DeleteDeploymentGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDeploymentGroup where
        toJSON DeleteDeploymentGroup'{..}
          = object
              (catMaybes
                 [Just ("applicationName" .= _ddgApplicationName),
                  Just
                    ("deploymentGroupName" .= _ddgDeploymentGroupName)])

instance ToPath DeleteDeploymentGroup where
        toPath = const "/"

instance ToQuery DeleteDeploymentGroup where
        toQuery = const mempty

-- | Represents the output of a DeleteDeploymentGroup operation.
--
--
--
-- /See:/ 'deleteDeploymentGroupResponse' smart constructor.
data DeleteDeploymentGroupResponse = DeleteDeploymentGroupResponse'
  { _ddgrsHooksNotCleanedUp :: !(Maybe [AutoScalingGroup])
  , _ddgrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeploymentGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddgrsHooksNotCleanedUp' - If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
--
-- * 'ddgrsResponseStatus' - -- | The response status code.
deleteDeploymentGroupResponse
    :: Int -- ^ 'ddgrsResponseStatus'
    -> DeleteDeploymentGroupResponse
deleteDeploymentGroupResponse pResponseStatus_ =
  DeleteDeploymentGroupResponse'
    {_ddgrsHooksNotCleanedUp = Nothing, _ddgrsResponseStatus = pResponseStatus_}


-- | If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
ddgrsHooksNotCleanedUp :: Lens' DeleteDeploymentGroupResponse [AutoScalingGroup]
ddgrsHooksNotCleanedUp = lens _ddgrsHooksNotCleanedUp (\ s a -> s{_ddgrsHooksNotCleanedUp = a}) . _Default . _Coerce

-- | -- | The response status code.
ddgrsResponseStatus :: Lens' DeleteDeploymentGroupResponse Int
ddgrsResponseStatus = lens _ddgrsResponseStatus (\ s a -> s{_ddgrsResponseStatus = a})

instance NFData DeleteDeploymentGroupResponse where

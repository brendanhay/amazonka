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
-- Module      : Network.AWS.CloudFormation.ContinueUpdateRollback
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specified stack that is in the @UPDATE_ROLLBACK_FAILED@ state, continues rolling it back to the @UPDATE_ROLLBACK_COMPLETE@ state. Depending on the cause of the failure, you can manually <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed fix the error> and continue the rollback. By continuing the rollback, you can return your stack to a working state (the @UPDATE_ROLLBACK_COMPLETE@ state), and then try to update the stack again.
--
--
-- A stack goes into the @UPDATE_ROLLBACK_FAILED@ state when AWS CloudFormation cannot roll back all changes after a failed stack update. For example, you might have a stack that is rolling back to an old database instance that was deleted outside of AWS CloudFormation. Because AWS CloudFormation doesn't know the database was deleted, it assumes that the database instance still exists and attempts to roll back to it, causing the update rollback to fail.
--
module Network.AWS.CloudFormation.ContinueUpdateRollback
    (
    -- * Creating a Request
      continueUpdateRollback
    , ContinueUpdateRollback
    -- * Request Lenses
    , curResourcesToSkip
    , curClientRequestToken
    , curRoleARN
    , curStackName

    -- * Destructuring the Response
    , continueUpdateRollbackResponse
    , ContinueUpdateRollbackResponse
    -- * Response Lenses
    , currsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'ContinueUpdateRollback' action.
--
--
--
-- /See:/ 'continueUpdateRollback' smart constructor.
data ContinueUpdateRollback = ContinueUpdateRollback'
  { _curResourcesToSkip    :: !(Maybe [Text])
  , _curClientRequestToken :: !(Maybe Text)
  , _curRoleARN            :: !(Maybe Text)
  , _curStackName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinueUpdateRollback' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'curResourcesToSkip' - A list of the logical IDs of the resources that AWS CloudFormation skips during the continue update rollback operation. You can specify only resources that are in the @UPDATE_FAILED@ state because a rollback failed. You can't specify resources that are in the @UPDATE_FAILED@ state for other reasons, for example, because an update was cancelled. To check why a resource update failed, use the 'DescribeStackResources' action, and view the resource status reason.  /Important:/ Specify this property to skip rolling back resources that AWS CloudFormation can't successfully roll back. We recommend that you <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot> resources before skipping them. AWS CloudFormation sets the status of the specified resources to @UPDATE_COMPLETE@ and continues to roll back the stack. After the rollback is complete, the state of the skipped resources will be inconsistent with the state of the resources in the stack template. Before performing another stack update, you must update the stack or resources to be consistent with each other. If you don't, subsequent stack updates might fail, and the stack will become unrecoverable.  Specify the minimum number of resources required to successfully roll back your stack. For example, a failed resource update might cause dependent resources to fail. In this case, it might not be necessary to skip the dependent resources.  To skip resources that are part of nested stacks, use the following format: @NestedStackName.ResourceLogicalID@ . If you want to specify the logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@ ) in the @ResourcesToSkip@ list, then its corresponding embedded stack must be in one of the following states: @DELETE_IN_PROGRESS@ , @DELETE_COMPLETE@ , or @DELETE_FAILED@ .
--
-- * 'curClientRequestToken' - A unique identifier for this @ContinueUpdateRollback@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to continue the rollback to a stack with the same name. You might retry @ContinueUpdateRollback@ requests to ensure that AWS CloudFormation successfully received them.
--
-- * 'curRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege. If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- * 'curStackName' - The name or the unique ID of the stack that you want to continue rolling back.
continueUpdateRollback
    :: Text -- ^ 'curStackName'
    -> ContinueUpdateRollback
continueUpdateRollback pStackName_ =
  ContinueUpdateRollback'
    { _curResourcesToSkip = Nothing
    , _curClientRequestToken = Nothing
    , _curRoleARN = Nothing
    , _curStackName = pStackName_
    }


-- | A list of the logical IDs of the resources that AWS CloudFormation skips during the continue update rollback operation. You can specify only resources that are in the @UPDATE_FAILED@ state because a rollback failed. You can't specify resources that are in the @UPDATE_FAILED@ state for other reasons, for example, because an update was cancelled. To check why a resource update failed, use the 'DescribeStackResources' action, and view the resource status reason.  /Important:/ Specify this property to skip rolling back resources that AWS CloudFormation can't successfully roll back. We recommend that you <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed troubleshoot> resources before skipping them. AWS CloudFormation sets the status of the specified resources to @UPDATE_COMPLETE@ and continues to roll back the stack. After the rollback is complete, the state of the skipped resources will be inconsistent with the state of the resources in the stack template. Before performing another stack update, you must update the stack or resources to be consistent with each other. If you don't, subsequent stack updates might fail, and the stack will become unrecoverable.  Specify the minimum number of resources required to successfully roll back your stack. For example, a failed resource update might cause dependent resources to fail. In this case, it might not be necessary to skip the dependent resources.  To skip resources that are part of nested stacks, use the following format: @NestedStackName.ResourceLogicalID@ . If you want to specify the logical ID of a stack resource (@Type: AWS::CloudFormation::Stack@ ) in the @ResourcesToSkip@ list, then its corresponding embedded stack must be in one of the following states: @DELETE_IN_PROGRESS@ , @DELETE_COMPLETE@ , or @DELETE_FAILED@ .
curResourcesToSkip :: Lens' ContinueUpdateRollback [Text]
curResourcesToSkip = lens _curResourcesToSkip (\ s a -> s{_curResourcesToSkip = a}) . _Default . _Coerce

-- | A unique identifier for this @ContinueUpdateRollback@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to continue the rollback to a stack with the same name. You might retry @ContinueUpdateRollback@ requests to ensure that AWS CloudFormation successfully received them.
curClientRequestToken :: Lens' ContinueUpdateRollback (Maybe Text)
curClientRequestToken = lens _curClientRequestToken (\ s a -> s{_curClientRequestToken = a})

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to roll back the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege. If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
curRoleARN :: Lens' ContinueUpdateRollback (Maybe Text)
curRoleARN = lens _curRoleARN (\ s a -> s{_curRoleARN = a})

-- | The name or the unique ID of the stack that you want to continue rolling back.
curStackName :: Lens' ContinueUpdateRollback Text
curStackName = lens _curStackName (\ s a -> s{_curStackName = a})

instance AWSRequest ContinueUpdateRollback where
        type Rs ContinueUpdateRollback =
             ContinueUpdateRollbackResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ContinueUpdateRollbackResult"
              (\ s h x ->
                 ContinueUpdateRollbackResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ContinueUpdateRollback where

instance NFData ContinueUpdateRollback where

instance ToHeaders ContinueUpdateRollback where
        toHeaders = const mempty

instance ToPath ContinueUpdateRollback where
        toPath = const "/"

instance ToQuery ContinueUpdateRollback where
        toQuery ContinueUpdateRollback'{..}
          = mconcat
              ["Action" =:
                 ("ContinueUpdateRollback" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "ResourcesToSkip" =:
                 toQuery
                   (toQueryList "member" <$> _curResourcesToSkip),
               "ClientRequestToken" =: _curClientRequestToken,
               "RoleARN" =: _curRoleARN,
               "StackName" =: _curStackName]

-- | The output for a 'ContinueUpdateRollback' action.
--
--
--
-- /See:/ 'continueUpdateRollbackResponse' smart constructor.
newtype ContinueUpdateRollbackResponse = ContinueUpdateRollbackResponse'
  { _currsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContinueUpdateRollbackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'currsResponseStatus' - -- | The response status code.
continueUpdateRollbackResponse
    :: Int -- ^ 'currsResponseStatus'
    -> ContinueUpdateRollbackResponse
continueUpdateRollbackResponse pResponseStatus_ =
  ContinueUpdateRollbackResponse' {_currsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
currsResponseStatus :: Lens' ContinueUpdateRollbackResponse Int
currsResponseStatus = lens _currsResponseStatus (\ s a -> s{_currsResponseStatus = a})

instance NFData ContinueUpdateRollbackResponse where

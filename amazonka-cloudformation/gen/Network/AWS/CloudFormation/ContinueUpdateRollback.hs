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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specified stack that is in the 'UPDATE_ROLLBACK_FAILED' state,
-- continues rolling it back to the 'UPDATE_ROLLBACK_COMPLETE' state.
-- Depending on the cause of the failure, you can manually
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed fix the error>
-- and continue the rollback. By continuing the rollback, you can return
-- your stack to a working state (the 'UPDATE_ROLLBACK_COMPLETE' state),
-- and then try to update the stack again.
--
-- A stack goes into the 'UPDATE_ROLLBACK_FAILED' state when AWS
-- CloudFormation cannot roll back all changes after a failed stack update.
-- For example, you might have a stack that is rolling back to an old
-- database instance that was deleted outside of AWS CloudFormation.
-- Because AWS CloudFormation doesn\'t know the database was deleted, it
-- assumes that the database instance still exists and attempts to roll
-- back to it, causing the update rollback to fail.
module Network.AWS.CloudFormation.ContinueUpdateRollback
    (
    -- * Creating a Request
      continueUpdateRollback
    , ContinueUpdateRollback
    -- * Request Lenses
    , curStackName

    -- * Destructuring the Response
    , continueUpdateRollbackResponse
    , ContinueUpdateRollbackResponse
    -- * Response Lenses
    , currsResponseStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the < ContinueUpdateRollback> action.
--
-- /See:/ 'continueUpdateRollback' smart constructor.
newtype ContinueUpdateRollback = ContinueUpdateRollback'
    { _curStackName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContinueUpdateRollback' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'curStackName'
continueUpdateRollback
    :: Text -- ^ 'curStackName'
    -> ContinueUpdateRollback
continueUpdateRollback pStackName_ =
    ContinueUpdateRollback'
    { _curStackName = pStackName_
    }

-- | The name or the unique ID of the stack that you want to continue rolling
-- back.
curStackName :: Lens' ContinueUpdateRollback Text
curStackName = lens _curStackName (\ s a -> s{_curStackName = a});

instance AWSRequest ContinueUpdateRollback where
        type Rs ContinueUpdateRollback =
             ContinueUpdateRollbackResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ContinueUpdateRollbackResult"
              (\ s h x ->
                 ContinueUpdateRollbackResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ContinueUpdateRollback

instance NFData ContinueUpdateRollback

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
               "StackName" =: _curStackName]

-- | The output for a < ContinueUpdateRollback> action.
--
-- /See:/ 'continueUpdateRollbackResponse' smart constructor.
newtype ContinueUpdateRollbackResponse = ContinueUpdateRollbackResponse'
    { _currsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContinueUpdateRollbackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'currsResponseStatus'
continueUpdateRollbackResponse
    :: Int -- ^ 'currsResponseStatus'
    -> ContinueUpdateRollbackResponse
continueUpdateRollbackResponse pResponseStatus_ =
    ContinueUpdateRollbackResponse'
    { _currsResponseStatus = pResponseStatus_
    }

-- | The response status code.
currsResponseStatus :: Lens' ContinueUpdateRollbackResponse Int
currsResponseStatus = lens _currsResponseStatus (\ s a -> s{_currsResponseStatus = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeletePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Auto Scaling policy.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeletePolicy.html AWS API Reference> for DeletePolicy.
module Network.AWS.AutoScaling.DeletePolicy
    (
    -- * Creating a Request
      DeletePolicy
    , deletePolicy
    -- * Request Lenses
    , dpAutoScalingGroupName
    , dpPolicyName

    -- * Destructuring the Response
    , DeletePolicyResponse
    , deletePolicyResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deletePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpAutoScalingGroupName'
--
-- * 'dpPolicyName'
data DeletePolicy = DeletePolicy'
    { _dpAutoScalingGroupName :: !(Maybe Text)
    , _dpPolicyName           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePolicy' smart constructor.
deletePolicy :: Text -> DeletePolicy
deletePolicy pPolicyName_ =
    DeletePolicy'
    { _dpAutoScalingGroupName = Nothing
    , _dpPolicyName = pPolicyName_
    }

-- | The name of the Auto Scaling group.
dpAutoScalingGroupName :: Lens' DeletePolicy (Maybe Text)
dpAutoScalingGroupName = lens _dpAutoScalingGroupName (\ s a -> s{_dpAutoScalingGroupName = a});

-- | The name or Amazon Resource Name (ARN) of the policy.
dpPolicyName :: Lens' DeletePolicy Text
dpPolicyName = lens _dpPolicyName (\ s a -> s{_dpPolicyName = a});

instance AWSRequest DeletePolicy where
        type Sv DeletePolicy = AutoScaling
        type Rs DeletePolicy = DeletePolicyResponse
        request = postQuery
        response = receiveNull DeletePolicyResponse'

instance ToHeaders DeletePolicy where
        toHeaders = const mempty

instance ToPath DeletePolicy where
        toPath = const "/"

instance ToQuery DeletePolicy where
        toQuery DeletePolicy'{..}
          = mconcat
              ["Action" =: ("DeletePolicy" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupName" =: _dpAutoScalingGroupName,
               "PolicyName" =: _dpPolicyName]

-- | /See:/ 'deletePolicyResponse' smart constructor.
data DeletePolicyResponse =
    DeletePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePolicyResponse' smart constructor.
deletePolicyResponse :: DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse'

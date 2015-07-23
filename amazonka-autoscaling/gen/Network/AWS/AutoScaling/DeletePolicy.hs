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
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeletePolicy.html>
module Network.AWS.AutoScaling.DeletePolicy
    (
    -- * Request
      DeletePolicy
    -- ** Request constructor
    , deletePolicy
    -- ** Request lenses
    , dprqAutoScalingGroupName
    , dprqPolicyName

    -- * Response
    , DeletePolicyResponse
    -- ** Response constructor
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
-- * 'dprqAutoScalingGroupName'
--
-- * 'dprqPolicyName'
data DeletePolicy = DeletePolicy'
    { _dprqAutoScalingGroupName :: !(Maybe Text)
    , _dprqPolicyName           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePolicy' smart constructor.
deletePolicy :: Text -> DeletePolicy
deletePolicy pPolicyName_ =
    DeletePolicy'
    { _dprqAutoScalingGroupName = Nothing
    , _dprqPolicyName = pPolicyName_
    }

-- | The name of the Auto Scaling group.
dprqAutoScalingGroupName :: Lens' DeletePolicy (Maybe Text)
dprqAutoScalingGroupName = lens _dprqAutoScalingGroupName (\ s a -> s{_dprqAutoScalingGroupName = a});

-- | The name or Amazon Resource Name (ARN) of the policy.
dprqPolicyName :: Lens' DeletePolicy Text
dprqPolicyName = lens _dprqPolicyName (\ s a -> s{_dprqPolicyName = a});

instance AWSRequest DeletePolicy where
        type Sv DeletePolicy = AutoScaling
        type Rs DeletePolicy = DeletePolicyResponse
        request = post
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
               "AutoScalingGroupName" =: _dprqAutoScalingGroupName,
               "PolicyName" =: _dprqPolicyName]

-- | /See:/ 'deletePolicyResponse' smart constructor.
data DeletePolicyResponse =
    DeletePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePolicyResponse' smart constructor.
deletePolicyResponse :: DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse'

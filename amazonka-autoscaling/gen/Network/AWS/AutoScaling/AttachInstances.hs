{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.AttachInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more EC2 instances to the specified Auto Scaling group.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/attach-instance-asg.html Attach EC2 Instances to Your Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AttachInstances.html AWS API Reference> for AttachInstances.
module Network.AWS.AutoScaling.AttachInstances
    (
    -- * Creating a Request
      AttachInstances
    , attachInstances
    -- * Request Lenses
    , aiInstanceIds
    , aiAutoScalingGroupName

    -- * Destructuring the Response
    , AttachInstancesResponse
    , attachInstancesResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiInstanceIds'
--
-- * 'aiAutoScalingGroupName'
data AttachInstances = AttachInstances'
    { _aiInstanceIds          :: !(Maybe [Text])
    , _aiAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachInstances' smart constructor.
attachInstances :: Text -> AttachInstances
attachInstances pAutoScalingGroupName_ =
    AttachInstances'
    { _aiInstanceIds = Nothing
    , _aiAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more EC2 instance IDs.
aiInstanceIds :: Lens' AttachInstances [Text]
aiInstanceIds = lens _aiInstanceIds (\ s a -> s{_aiInstanceIds = a}) . _Default . _Coerce;

-- | The name of the group.
aiAutoScalingGroupName :: Lens' AttachInstances Text
aiAutoScalingGroupName = lens _aiAutoScalingGroupName (\ s a -> s{_aiAutoScalingGroupName = a});

instance AWSRequest AttachInstances where
        type Sv AttachInstances = AutoScaling
        type Rs AttachInstances = AttachInstancesResponse
        request = postQuery
        response = receiveNull AttachInstancesResponse'

instance ToHeaders AttachInstances where
        toHeaders = const mempty

instance ToPath AttachInstances where
        toPath = const "/"

instance ToQuery AttachInstances where
        toQuery AttachInstances'{..}
          = mconcat
              ["Action" =: ("AttachInstances" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceIds" =:
                 toQuery (toQueryList "member" <$> _aiInstanceIds),
               "AutoScalingGroupName" =: _aiAutoScalingGroupName]

-- | /See:/ 'attachInstancesResponse' smart constructor.
data AttachInstancesResponse =
    AttachInstancesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachInstancesResponse' smart constructor.
attachInstancesResponse :: AttachInstancesResponse
attachInstancesResponse = AttachInstancesResponse'

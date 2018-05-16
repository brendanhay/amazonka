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
-- Module      : Network.AWS.AutoScaling.EnterStandby
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances into the standby state.
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-enter-exit-standby.html Temporarily Removing Instances from Your Auto Scaling Group> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.EnterStandby
    (
    -- * Creating a Request
      enterStandby
    , EnterStandby
    -- * Request Lenses
    , esInstanceIds
    , esAutoScalingGroupName
    , esShouldDecrementDesiredCapacity

    -- * Destructuring the Response
    , enterStandbyResponse
    , EnterStandbyResponse
    -- * Response Lenses
    , ersActivities
    , ersResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enterStandby' smart constructor.
data EnterStandby = EnterStandby'
  { _esInstanceIds                    :: !(Maybe [Text])
  , _esAutoScalingGroupName           :: !Text
  , _esShouldDecrementDesiredCapacity :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnterStandby' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esInstanceIds' - The IDs of the instances. You can specify up to 20 instances.
--
-- * 'esAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'esShouldDecrementDesiredCapacity' - Indicates whether to decrement the desired capacity of the Auto Scaling group by the number of instances moved to @Standby@ mode.
enterStandby
    :: Text -- ^ 'esAutoScalingGroupName'
    -> Bool -- ^ 'esShouldDecrementDesiredCapacity'
    -> EnterStandby
enterStandby pAutoScalingGroupName_ pShouldDecrementDesiredCapacity_ =
  EnterStandby'
    { _esInstanceIds = Nothing
    , _esAutoScalingGroupName = pAutoScalingGroupName_
    , _esShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
    }


-- | The IDs of the instances. You can specify up to 20 instances.
esInstanceIds :: Lens' EnterStandby [Text]
esInstanceIds = lens _esInstanceIds (\ s a -> s{_esInstanceIds = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
esAutoScalingGroupName :: Lens' EnterStandby Text
esAutoScalingGroupName = lens _esAutoScalingGroupName (\ s a -> s{_esAutoScalingGroupName = a})

-- | Indicates whether to decrement the desired capacity of the Auto Scaling group by the number of instances moved to @Standby@ mode.
esShouldDecrementDesiredCapacity :: Lens' EnterStandby Bool
esShouldDecrementDesiredCapacity = lens _esShouldDecrementDesiredCapacity (\ s a -> s{_esShouldDecrementDesiredCapacity = a})

instance AWSRequest EnterStandby where
        type Rs EnterStandby = EnterStandbyResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "EnterStandbyResult"
              (\ s h x ->
                 EnterStandbyResponse' <$>
                   (x .@? "Activities" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable EnterStandby where

instance NFData EnterStandby where

instance ToHeaders EnterStandby where
        toHeaders = const mempty

instance ToPath EnterStandby where
        toPath = const "/"

instance ToQuery EnterStandby where
        toQuery EnterStandby'{..}
          = mconcat
              ["Action" =: ("EnterStandby" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceIds" =:
                 toQuery (toQueryList "member" <$> _esInstanceIds),
               "AutoScalingGroupName" =: _esAutoScalingGroupName,
               "ShouldDecrementDesiredCapacity" =:
                 _esShouldDecrementDesiredCapacity]

-- | /See:/ 'enterStandbyResponse' smart constructor.
data EnterStandbyResponse = EnterStandbyResponse'
  { _ersActivities     :: !(Maybe [Activity])
  , _ersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnterStandbyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ersActivities' - The activities related to moving instances into @Standby@ mode.
--
-- * 'ersResponseStatus' - -- | The response status code.
enterStandbyResponse
    :: Int -- ^ 'ersResponseStatus'
    -> EnterStandbyResponse
enterStandbyResponse pResponseStatus_ =
  EnterStandbyResponse'
    {_ersActivities = Nothing, _ersResponseStatus = pResponseStatus_}


-- | The activities related to moving instances into @Standby@ mode.
ersActivities :: Lens' EnterStandbyResponse [Activity]
ersActivities = lens _ersActivities (\ s a -> s{_ersActivities = a}) . _Default . _Coerce

-- | -- | The response status code.
ersResponseStatus :: Lens' EnterStandbyResponse Int
ersResponseStatus = lens _ersResponseStatus (\ s a -> s{_ersResponseStatus = a})

instance NFData EnterStandbyResponse where

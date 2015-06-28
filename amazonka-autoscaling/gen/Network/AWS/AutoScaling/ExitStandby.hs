{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.ExitStandby
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Moves the specified instances out of @Standby@ mode.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingInServiceState.html Auto Scaling InService State>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ExitStandby.html>
module Network.AWS.AutoScaling.ExitStandby
    (
    -- * Request
      ExitStandby
    -- ** Request constructor
    , exitStandby
    -- ** Request lenses
    , exiInstanceIds
    , exiAutoScalingGroupName

    -- * Response
    , ExitStandbyResponse
    -- ** Response constructor
    , exitStandbyResponse
    -- ** Response lenses
    , exiActivities
    , exiStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'exitStandby' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'exiInstanceIds'
--
-- * 'exiAutoScalingGroupName'
data ExitStandby = ExitStandby'
    { _exiInstanceIds          :: !(Maybe [Text])
    , _exiAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show)

-- | 'ExitStandby' smart constructor.
exitStandby :: Text -> ExitStandby
exitStandby pAutoScalingGroupName =
    ExitStandby'
    { _exiInstanceIds = Nothing
    , _exiAutoScalingGroupName = pAutoScalingGroupName
    }

-- | One or more instance IDs. You must specify at least one instance ID.
exiInstanceIds :: Lens' ExitStandby [Text]
exiInstanceIds = lens _exiInstanceIds (\ s a -> s{_exiInstanceIds = a}) . _Default;

-- | The name of the Auto Scaling group.
exiAutoScalingGroupName :: Lens' ExitStandby Text
exiAutoScalingGroupName = lens _exiAutoScalingGroupName (\ s a -> s{_exiAutoScalingGroupName = a});

instance AWSRequest ExitStandby where
        type Sv ExitStandby = AutoScaling
        type Rs ExitStandby = ExitStandbyResponse
        request = post
        response
          = receiveXMLWrapper "ExitStandbyResult"
              (\ s h x ->
                 ExitStandbyResponse' <$>
                   (x .@? "Activities" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure s))

instance ToHeaders ExitStandby where
        toHeaders = const mempty

instance ToPath ExitStandby where
        toPath = const "/"

instance ToQuery ExitStandby where
        toQuery ExitStandby'{..}
          = mconcat
              ["Action" =: ("ExitStandby" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceIds" =:
                 toQuery (toQueryList "member" <$> _exiInstanceIds),
               "AutoScalingGroupName" =: _exiAutoScalingGroupName]

-- | /See:/ 'exitStandbyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'exiActivities'
--
-- * 'exiStatus'
data ExitStandbyResponse = ExitStandbyResponse'
    { _exiActivities :: !(Maybe [Activity])
    , _exiStatus     :: !Status
    } deriving (Eq,Show)

-- | 'ExitStandbyResponse' smart constructor.
exitStandbyResponse :: Status -> ExitStandbyResponse
exitStandbyResponse pStatus =
    ExitStandbyResponse'
    { _exiActivities = Nothing
    , _exiStatus = pStatus
    }

-- | The activities related to moving instances out of @Standby@ mode.
exiActivities :: Lens' ExitStandbyResponse [Activity]
exiActivities = lens _exiActivities (\ s a -> s{_exiActivities = a}) . _Default;

-- | FIXME: Undocumented member.
exiStatus :: Lens' ExitStandbyResponse Status
exiStatus = lens _exiStatus (\ s a -> s{_exiStatus = a});

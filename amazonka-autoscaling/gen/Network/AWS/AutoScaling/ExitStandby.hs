{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.ExitStandby
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances out of @Standby@ mode.
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
    , erqInstanceIds
    , erqAutoScalingGroupName

    -- * Response
    , ExitStandbyResponse
    -- ** Response constructor
    , exitStandbyResponse
    -- ** Response lenses
    , ersActivities
    , ersStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'exitStandby' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erqInstanceIds'
--
-- * 'erqAutoScalingGroupName'
data ExitStandby = ExitStandby'
    { _erqInstanceIds          :: !(Maybe [Text])
    , _erqAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ExitStandby' smart constructor.
exitStandby :: Text -> ExitStandby
exitStandby pAutoScalingGroupName =
    ExitStandby'
    { _erqInstanceIds = Nothing
    , _erqAutoScalingGroupName = pAutoScalingGroupName
    }

-- | One or more instance IDs. You must specify at least one instance ID.
erqInstanceIds :: Lens' ExitStandby [Text]
erqInstanceIds = lens _erqInstanceIds (\ s a -> s{_erqInstanceIds = a}) . _Default;

-- | The name of the Auto Scaling group.
erqAutoScalingGroupName :: Lens' ExitStandby Text
erqAutoScalingGroupName = lens _erqAutoScalingGroupName (\ s a -> s{_erqAutoScalingGroupName = a});

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
                     <*> (pure (fromEnum s)))

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
                 toQuery (toQueryList "member" <$> _erqInstanceIds),
               "AutoScalingGroupName" =: _erqAutoScalingGroupName]

-- | /See:/ 'exitStandbyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ersActivities'
--
-- * 'ersStatus'
data ExitStandbyResponse = ExitStandbyResponse'
    { _ersActivities :: !(Maybe [Activity])
    , _ersStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ExitStandbyResponse' smart constructor.
exitStandbyResponse :: Int -> ExitStandbyResponse
exitStandbyResponse pStatus =
    ExitStandbyResponse'
    { _ersActivities = Nothing
    , _ersStatus = pStatus
    }

-- | The activities related to moving instances out of @Standby@ mode.
ersActivities :: Lens' ExitStandbyResponse [Activity]
ersActivities = lens _ersActivities (\ s a -> s{_ersActivities = a}) . _Default;

-- | FIXME: Undocumented member.
ersStatus :: Lens' ExitStandbyResponse Int
ersStatus = lens _ersStatus (\ s a -> s{_ersStatus = a});

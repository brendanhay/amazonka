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
-- Module      : Network.AWS.AutoScaling.DisableMetricsCollection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables monitoring of the specified metrics for the specified Auto
-- Scaling group.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DisableMetricsCollection.html AWS API Reference> for DisableMetricsCollection.
module Network.AWS.AutoScaling.DisableMetricsCollection
    (
    -- * Creating a Request
      disableMetricsCollection
    , DisableMetricsCollection
    -- * Request Lenses
    , dmcMetrics
    , dmcAutoScalingGroupName

    -- * Destructuring the Response
    , disableMetricsCollectionResponse
    , DisableMetricsCollectionResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disableMetricsCollection' smart constructor.
data DisableMetricsCollection = DisableMetricsCollection'
    { _dmcMetrics              :: !(Maybe [Text])
    , _dmcAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableMetricsCollection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmcMetrics'
--
-- * 'dmcAutoScalingGroupName'
disableMetricsCollection
    :: Text -- ^ 'dmcAutoScalingGroupName'
    -> DisableMetricsCollection
disableMetricsCollection pAutoScalingGroupName_ =
    DisableMetricsCollection'
    { _dmcMetrics = Nothing
    , _dmcAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more metrics. If you omit this parameter, all metrics are
-- disabled.
--
-- -   'GroupMinSize'
--
-- -   'GroupMaxSize'
--
-- -   'GroupDesiredCapacity'
--
-- -   'GroupInServiceInstances'
--
-- -   'GroupPendingInstances'
--
-- -   'GroupStandbyInstances'
--
-- -   'GroupTerminatingInstances'
--
-- -   'GroupTotalInstances'
--
dmcMetrics :: Lens' DisableMetricsCollection [Text]
dmcMetrics = lens _dmcMetrics (\ s a -> s{_dmcMetrics = a}) . _Default . _Coerce;

-- | The name or Amazon Resource Name (ARN) of the group.
dmcAutoScalingGroupName :: Lens' DisableMetricsCollection Text
dmcAutoScalingGroupName = lens _dmcAutoScalingGroupName (\ s a -> s{_dmcAutoScalingGroupName = a});

instance AWSRequest DisableMetricsCollection where
        type Rs DisableMetricsCollection =
             DisableMetricsCollectionResponse
        request = postQuery autoScaling
        response
          = receiveNull DisableMetricsCollectionResponse'

instance ToHeaders DisableMetricsCollection where
        toHeaders = const mempty

instance ToPath DisableMetricsCollection where
        toPath = const "/"

instance ToQuery DisableMetricsCollection where
        toQuery DisableMetricsCollection'{..}
          = mconcat
              ["Action" =:
                 ("DisableMetricsCollection" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "Metrics" =:
                 toQuery (toQueryList "member" <$> _dmcMetrics),
               "AutoScalingGroupName" =: _dmcAutoScalingGroupName]

-- | /See:/ 'disableMetricsCollectionResponse' smart constructor.
data DisableMetricsCollectionResponse =
    DisableMetricsCollectionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableMetricsCollectionResponse' with the minimum fields required to make a request.
--
disableMetricsCollectionResponse
    :: DisableMetricsCollectionResponse
disableMetricsCollectionResponse = DisableMetricsCollectionResponse'

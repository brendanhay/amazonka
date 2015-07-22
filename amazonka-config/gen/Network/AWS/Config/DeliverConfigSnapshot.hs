{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeliverConfigSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Schedules delivery of a configuration snapshot to the Amazon S3 bucket
-- in the specified delivery channel. After the delivery has started, AWS
-- Config sends following notifications using an Amazon SNS topic that you
-- have specified.
--
-- -   Notification of starting the delivery.
-- -   Notification of delivery completed, if the delivery was successfully
--     completed.
-- -   Notification of delivery failure, if the delivery failed to
--     complete.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_DeliverConfigSnapshot.html>
module Network.AWS.Config.DeliverConfigSnapshot
    (
    -- * Request
      DeliverConfigSnapshot
    -- ** Request constructor
    , deliverConfigSnapshot
    -- ** Request lenses
    , dcsrqDeliveryChannelName

    -- * Response
    , DeliverConfigSnapshotResponse
    -- ** Response constructor
    , deliverConfigSnapshotResponse
    -- ** Response lenses
    , dcsrsConfigSnapshotId
    , dcsrsStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DeliverConfigSnapshot action.
--
-- /See:/ 'deliverConfigSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsrqDeliveryChannelName'
newtype DeliverConfigSnapshot = DeliverConfigSnapshot'
    { _dcsrqDeliveryChannelName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeliverConfigSnapshot' smart constructor.
deliverConfigSnapshot :: Text -> DeliverConfigSnapshot
deliverConfigSnapshot pDeliveryChannelName =
    DeliverConfigSnapshot'
    { _dcsrqDeliveryChannelName = pDeliveryChannelName
    }

-- | The name of the delivery channel through which the snapshot is
-- delivered.
dcsrqDeliveryChannelName :: Lens' DeliverConfigSnapshot Text
dcsrqDeliveryChannelName = lens _dcsrqDeliveryChannelName (\ s a -> s{_dcsrqDeliveryChannelName = a});

instance AWSRequest DeliverConfigSnapshot where
        type Sv DeliverConfigSnapshot = Config
        type Rs DeliverConfigSnapshot =
             DeliverConfigSnapshotResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeliverConfigSnapshotResponse' <$>
                   (x .?> "configSnapshotId") <*> (pure (fromEnum s)))

instance ToHeaders DeliverConfigSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeliverConfigSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeliverConfigSnapshot where
        toJSON DeliverConfigSnapshot'{..}
          = object
              ["deliveryChannelName" .= _dcsrqDeliveryChannelName]

instance ToPath DeliverConfigSnapshot where
        toPath = const "/"

instance ToQuery DeliverConfigSnapshot where
        toQuery = const mempty

-- | The output for the DeliverConfigSnapshot action in JSON format.
--
-- /See:/ 'deliverConfigSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsrsConfigSnapshotId'
--
-- * 'dcsrsStatus'
data DeliverConfigSnapshotResponse = DeliverConfigSnapshotResponse'
    { _dcsrsConfigSnapshotId :: !(Maybe Text)
    , _dcsrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeliverConfigSnapshotResponse' smart constructor.
deliverConfigSnapshotResponse :: Int -> DeliverConfigSnapshotResponse
deliverConfigSnapshotResponse pStatus =
    DeliverConfigSnapshotResponse'
    { _dcsrsConfigSnapshotId = Nothing
    , _dcsrsStatus = pStatus
    }

-- | The ID of the snapshot that is being created.
dcsrsConfigSnapshotId :: Lens' DeliverConfigSnapshotResponse (Maybe Text)
dcsrsConfigSnapshotId = lens _dcsrsConfigSnapshotId (\ s a -> s{_dcsrsConfigSnapshotId = a});

-- | FIXME: Undocumented member.
dcsrsStatus :: Lens' DeliverConfigSnapshotResponse Int
dcsrsStatus = lens _dcsrsStatus (\ s a -> s{_dcsrsStatus = a});

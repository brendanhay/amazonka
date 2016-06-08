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
-- Module      : Network.AWS.KMS.ScheduleKeyDeletion
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules the deletion of a customer master key (CMK). You may provide a waiting period, specified in days, before deletion occurs. If you do not provide a waiting period, the default period of 30 days is used. When this operation is successful, the state of the CMK changes to 'PendingDeletion'. Before the waiting period ends, you can use < CancelKeyDeletion> to cancel the deletion of the CMK. After the waiting period ends, AWS KMS deletes the CMK and all AWS KMS data associated with it, including all aliases that point to it.
--
-- Deleting a CMK is a destructive and potentially dangerous operation. When a CMK is deleted, all data that was encrypted under the CMK is rendered unrecoverable. To restrict the use of a CMK without deleting it, use < DisableKey>.
--
-- For more information about scheduling a CMK for deletion, go to <http://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting Customer Master Keys> in the /AWS Key Management Service Developer Guide/.
module Network.AWS.KMS.ScheduleKeyDeletion
    (
    -- * Creating a Request
      scheduleKeyDeletion
    , ScheduleKeyDeletion
    -- * Request Lenses
    , skdPendingWindowInDays
    , skdKeyId

    -- * Destructuring the Response
    , scheduleKeyDeletionResponse
    , ScheduleKeyDeletionResponse
    -- * Response Lenses
    , skdrsKeyId
    , skdrsDeletionDate
    , skdrsResponseStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.KMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'scheduleKeyDeletion' smart constructor.
data ScheduleKeyDeletion = ScheduleKeyDeletion'
    { _skdPendingWindowInDays :: !(Maybe Nat)
    , _skdKeyId               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScheduleKeyDeletion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skdPendingWindowInDays'
--
-- * 'skdKeyId'
scheduleKeyDeletion
    :: Text -- ^ 'skdKeyId'
    -> ScheduleKeyDeletion
scheduleKeyDeletion pKeyId_ =
    ScheduleKeyDeletion'
    { _skdPendingWindowInDays = Nothing
    , _skdKeyId = pKeyId_
    }

-- | The waiting period, specified in number of days. After the waiting period ends, AWS KMS deletes the customer master key (CMK).
--
-- This value is optional. If you include a value, it must be between 7 and 30, inclusive. If you do not include a value, it defaults to 30.
skdPendingWindowInDays :: Lens' ScheduleKeyDeletion (Maybe Natural)
skdPendingWindowInDays = lens _skdPendingWindowInDays (\ s a -> s{_skdPendingWindowInDays = a}) . mapping _Nat;

-- | The unique identifier for the customer master key (CMK) to delete.
--
-- To specify this value, use the unique key ID or the Amazon Resource Name (ARN) of the CMK. Examples:
--
-- -   Unique key ID: 1234abcd-12ab-34cd-56ef-1234567890ab
-- -   Key ARN: arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab
--
-- To obtain the unique key ID and key ARN for a given CMK, use < ListKeys> or < DescribeKey>.
skdKeyId :: Lens' ScheduleKeyDeletion Text
skdKeyId = lens _skdKeyId (\ s a -> s{_skdKeyId = a});

instance AWSRequest ScheduleKeyDeletion where
        type Rs ScheduleKeyDeletion =
             ScheduleKeyDeletionResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 ScheduleKeyDeletionResponse' <$>
                   (x .?> "KeyId") <*> (x .?> "DeletionDate") <*>
                     (pure (fromEnum s)))

instance Hashable ScheduleKeyDeletion

instance NFData ScheduleKeyDeletion

instance ToHeaders ScheduleKeyDeletion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ScheduleKeyDeletion" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ScheduleKeyDeletion where
        toJSON ScheduleKeyDeletion'{..}
          = object
              (catMaybes
                 [("PendingWindowInDays" .=) <$>
                    _skdPendingWindowInDays,
                  Just ("KeyId" .= _skdKeyId)])

instance ToPath ScheduleKeyDeletion where
        toPath = const "/"

instance ToQuery ScheduleKeyDeletion where
        toQuery = const mempty

-- | /See:/ 'scheduleKeyDeletionResponse' smart constructor.
data ScheduleKeyDeletionResponse = ScheduleKeyDeletionResponse'
    { _skdrsKeyId          :: !(Maybe Text)
    , _skdrsDeletionDate   :: !(Maybe POSIX)
    , _skdrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ScheduleKeyDeletionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skdrsKeyId'
--
-- * 'skdrsDeletionDate'
--
-- * 'skdrsResponseStatus'
scheduleKeyDeletionResponse
    :: Int -- ^ 'skdrsResponseStatus'
    -> ScheduleKeyDeletionResponse
scheduleKeyDeletionResponse pResponseStatus_ =
    ScheduleKeyDeletionResponse'
    { _skdrsKeyId = Nothing
    , _skdrsDeletionDate = Nothing
    , _skdrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier of the customer master key (CMK) for which deletion is scheduled.
skdrsKeyId :: Lens' ScheduleKeyDeletionResponse (Maybe Text)
skdrsKeyId = lens _skdrsKeyId (\ s a -> s{_skdrsKeyId = a});

-- | The date and time after which AWS KMS deletes the customer master key (CMK).
skdrsDeletionDate :: Lens' ScheduleKeyDeletionResponse (Maybe UTCTime)
skdrsDeletionDate = lens _skdrsDeletionDate (\ s a -> s{_skdrsDeletionDate = a}) . mapping _Time;

-- | The response status code.
skdrsResponseStatus :: Lens' ScheduleKeyDeletionResponse Int
skdrsResponseStatus = lens _skdrsResponseStatus (\ s a -> s{_skdrsResponseStatus = a});

instance NFData ScheduleKeyDeletionResponse

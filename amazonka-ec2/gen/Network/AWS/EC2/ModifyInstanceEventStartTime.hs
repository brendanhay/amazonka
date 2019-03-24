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
-- Module      : Network.AWS.EC2.ModifyInstanceEventStartTime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the start time for a scheduled Amazon EC2 instance event.
--
--
module Network.AWS.EC2.ModifyInstanceEventStartTime
    (
    -- * Creating a Request
      modifyInstanceEventStartTime
    , ModifyInstanceEventStartTime
    -- * Request Lenses
    , miestDryRun
    , miestInstanceId
    , miestInstanceEventId
    , miestNotBefore

    -- * Destructuring the Response
    , modifyInstanceEventStartTimeResponse
    , ModifyInstanceEventStartTimeResponse
    -- * Response Lenses
    , miestrsEvent
    , miestrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyInstanceEventStartTime' smart constructor.
data ModifyInstanceEventStartTime = ModifyInstanceEventStartTime'
  { _miestDryRun          :: !(Maybe Bool)
  , _miestInstanceId      :: !Text
  , _miestInstanceEventId :: !Text
  , _miestNotBefore       :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceEventStartTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miestDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation.@
--
-- * 'miestInstanceId' - The ID of the instance with the scheduled event.
--
-- * 'miestInstanceEventId' - The ID of the event whose date and time you are modifying.
--
-- * 'miestNotBefore' - The new date and time when the event will take place.
modifyInstanceEventStartTime
    :: Text -- ^ 'miestInstanceId'
    -> Text -- ^ 'miestInstanceEventId'
    -> UTCTime -- ^ 'miestNotBefore'
    -> ModifyInstanceEventStartTime
modifyInstanceEventStartTime pInstanceId_ pInstanceEventId_ pNotBefore_ =
  ModifyInstanceEventStartTime'
    { _miestDryRun = Nothing
    , _miestInstanceId = pInstanceId_
    , _miestInstanceEventId = pInstanceEventId_
    , _miestNotBefore = _Time # pNotBefore_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation.@
miestDryRun :: Lens' ModifyInstanceEventStartTime (Maybe Bool)
miestDryRun = lens _miestDryRun (\ s a -> s{_miestDryRun = a})

-- | The ID of the instance with the scheduled event.
miestInstanceId :: Lens' ModifyInstanceEventStartTime Text
miestInstanceId = lens _miestInstanceId (\ s a -> s{_miestInstanceId = a})

-- | The ID of the event whose date and time you are modifying.
miestInstanceEventId :: Lens' ModifyInstanceEventStartTime Text
miestInstanceEventId = lens _miestInstanceEventId (\ s a -> s{_miestInstanceEventId = a})

-- | The new date and time when the event will take place.
miestNotBefore :: Lens' ModifyInstanceEventStartTime UTCTime
miestNotBefore = lens _miestNotBefore (\ s a -> s{_miestNotBefore = a}) . _Time

instance AWSRequest ModifyInstanceEventStartTime
         where
        type Rs ModifyInstanceEventStartTime =
             ModifyInstanceEventStartTimeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyInstanceEventStartTimeResponse' <$>
                   (x .@? "event") <*> (pure (fromEnum s)))

instance Hashable ModifyInstanceEventStartTime where

instance NFData ModifyInstanceEventStartTime where

instance ToHeaders ModifyInstanceEventStartTime where
        toHeaders = const mempty

instance ToPath ModifyInstanceEventStartTime where
        toPath = const "/"

instance ToQuery ModifyInstanceEventStartTime where
        toQuery ModifyInstanceEventStartTime'{..}
          = mconcat
              ["Action" =:
                 ("ModifyInstanceEventStartTime" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _miestDryRun,
               "InstanceId" =: _miestInstanceId,
               "InstanceEventId" =: _miestInstanceEventId,
               "NotBefore" =: _miestNotBefore]

-- | /See:/ 'modifyInstanceEventStartTimeResponse' smart constructor.
data ModifyInstanceEventStartTimeResponse = ModifyInstanceEventStartTimeResponse'
  { _miestrsEvent          :: !(Maybe InstanceStatusEvent)
  , _miestrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceEventStartTimeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miestrsEvent' - Undocumented member.
--
-- * 'miestrsResponseStatus' - -- | The response status code.
modifyInstanceEventStartTimeResponse
    :: Int -- ^ 'miestrsResponseStatus'
    -> ModifyInstanceEventStartTimeResponse
modifyInstanceEventStartTimeResponse pResponseStatus_ =
  ModifyInstanceEventStartTimeResponse'
    {_miestrsEvent = Nothing, _miestrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
miestrsEvent :: Lens' ModifyInstanceEventStartTimeResponse (Maybe InstanceStatusEvent)
miestrsEvent = lens _miestrsEvent (\ s a -> s{_miestrsEvent = a})

-- | -- | The response status code.
miestrsResponseStatus :: Lens' ModifyInstanceEventStartTimeResponse Int
miestrsResponseStatus = lens _miestrsResponseStatus (\ s a -> s{_miestrsResponseStatus = a})

instance NFData ModifyInstanceEventStartTimeResponse
         where

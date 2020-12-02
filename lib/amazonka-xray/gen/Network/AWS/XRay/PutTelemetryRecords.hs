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
-- Module      : Network.AWS.XRay.PutTelemetryRecords
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by the AWS X-Ray daemon to upload telemetry.
--
--
module Network.AWS.XRay.PutTelemetryRecords
    (
    -- * Creating a Request
      putTelemetryRecords
    , PutTelemetryRecords
    -- * Request Lenses
    , ptrHostname
    , ptrEC2InstanceId
    , ptrResourceARN
    , ptrTelemetryRecords

    -- * Destructuring the Response
    , putTelemetryRecordsResponse
    , PutTelemetryRecordsResponse
    -- * Response Lenses
    , ptrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'putTelemetryRecords' smart constructor.
data PutTelemetryRecords = PutTelemetryRecords'
  { _ptrHostname         :: !(Maybe Text)
  , _ptrEC2InstanceId    :: !(Maybe Text)
  , _ptrResourceARN      :: !(Maybe Text)
  , _ptrTelemetryRecords :: ![TelemetryRecord]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutTelemetryRecords' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptrHostname' -
--
-- * 'ptrEC2InstanceId' -
--
-- * 'ptrResourceARN' -
--
-- * 'ptrTelemetryRecords' -
putTelemetryRecords
    :: PutTelemetryRecords
putTelemetryRecords =
  PutTelemetryRecords'
    { _ptrHostname = Nothing
    , _ptrEC2InstanceId = Nothing
    , _ptrResourceARN = Nothing
    , _ptrTelemetryRecords = mempty
    }


-- |
ptrHostname :: Lens' PutTelemetryRecords (Maybe Text)
ptrHostname = lens _ptrHostname (\ s a -> s{_ptrHostname = a})

-- |
ptrEC2InstanceId :: Lens' PutTelemetryRecords (Maybe Text)
ptrEC2InstanceId = lens _ptrEC2InstanceId (\ s a -> s{_ptrEC2InstanceId = a})

-- |
ptrResourceARN :: Lens' PutTelemetryRecords (Maybe Text)
ptrResourceARN = lens _ptrResourceARN (\ s a -> s{_ptrResourceARN = a})

-- |
ptrTelemetryRecords :: Lens' PutTelemetryRecords [TelemetryRecord]
ptrTelemetryRecords = lens _ptrTelemetryRecords (\ s a -> s{_ptrTelemetryRecords = a}) . _Coerce

instance AWSRequest PutTelemetryRecords where
        type Rs PutTelemetryRecords =
             PutTelemetryRecordsResponse
        request = postJSON xRay
        response
          = receiveEmpty
              (\ s h x ->
                 PutTelemetryRecordsResponse' <$> (pure (fromEnum s)))

instance Hashable PutTelemetryRecords where

instance NFData PutTelemetryRecords where

instance ToHeaders PutTelemetryRecords where
        toHeaders = const mempty

instance ToJSON PutTelemetryRecords where
        toJSON PutTelemetryRecords'{..}
          = object
              (catMaybes
                 [("Hostname" .=) <$> _ptrHostname,
                  ("EC2InstanceId" .=) <$> _ptrEC2InstanceId,
                  ("ResourceARN" .=) <$> _ptrResourceARN,
                  Just ("TelemetryRecords" .= _ptrTelemetryRecords)])

instance ToPath PutTelemetryRecords where
        toPath = const "/TelemetryRecords"

instance ToQuery PutTelemetryRecords where
        toQuery = const mempty

-- | /See:/ 'putTelemetryRecordsResponse' smart constructor.
newtype PutTelemetryRecordsResponse = PutTelemetryRecordsResponse'
  { _ptrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutTelemetryRecordsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptrrsResponseStatus' - -- | The response status code.
putTelemetryRecordsResponse
    :: Int -- ^ 'ptrrsResponseStatus'
    -> PutTelemetryRecordsResponse
putTelemetryRecordsResponse pResponseStatus_ =
  PutTelemetryRecordsResponse' {_ptrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ptrrsResponseStatus :: Lens' PutTelemetryRecordsResponse Int
ptrrsResponseStatus = lens _ptrrsResponseStatus (\ s a -> s{_ptrrsResponseStatus = a})

instance NFData PutTelemetryRecordsResponse where

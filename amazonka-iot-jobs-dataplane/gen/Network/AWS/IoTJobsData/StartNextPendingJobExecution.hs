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
-- Module      : Network.AWS.IoTJobsData.StartNextPendingJobExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets and starts the next pending (status IN_PROGRESS or QUEUED) job execution for a thing.
--
--
module Network.AWS.IoTJobsData.StartNextPendingJobExecution
    (
    -- * Creating a Request
      startNextPendingJobExecution
    , StartNextPendingJobExecution
    -- * Request Lenses
    , snpjeStatusDetails
    , snpjeThingName

    -- * Destructuring the Response
    , startNextPendingJobExecutionResponse
    , StartNextPendingJobExecutionResponse
    -- * Response Lenses
    , snpjersExecution
    , snpjersResponseStatus
    ) where

import Network.AWS.IoTJobsData.Types
import Network.AWS.IoTJobsData.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startNextPendingJobExecution' smart constructor.
data StartNextPendingJobExecution = StartNextPendingJobExecution'
  { _snpjeStatusDetails :: !(Maybe (Map Text Text))
  , _snpjeThingName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartNextPendingJobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snpjeStatusDetails' - A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
--
-- * 'snpjeThingName' - The name of the thing associated with the device.
startNextPendingJobExecution
    :: Text -- ^ 'snpjeThingName'
    -> StartNextPendingJobExecution
startNextPendingJobExecution pThingName_ =
  StartNextPendingJobExecution'
    {_snpjeStatusDetails = Nothing, _snpjeThingName = pThingName_}


-- | A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged.
snpjeStatusDetails :: Lens' StartNextPendingJobExecution (HashMap Text Text)
snpjeStatusDetails = lens _snpjeStatusDetails (\ s a -> s{_snpjeStatusDetails = a}) . _Default . _Map

-- | The name of the thing associated with the device.
snpjeThingName :: Lens' StartNextPendingJobExecution Text
snpjeThingName = lens _snpjeThingName (\ s a -> s{_snpjeThingName = a})

instance AWSRequest StartNextPendingJobExecution
         where
        type Rs StartNextPendingJobExecution =
             StartNextPendingJobExecutionResponse
        request = putJSON ioTJobsData
        response
          = receiveJSON
              (\ s h x ->
                 StartNextPendingJobExecutionResponse' <$>
                   (x .?> "execution") <*> (pure (fromEnum s)))

instance Hashable StartNextPendingJobExecution where

instance NFData StartNextPendingJobExecution where

instance ToHeaders StartNextPendingJobExecution where
        toHeaders = const mempty

instance ToJSON StartNextPendingJobExecution where
        toJSON StartNextPendingJobExecution'{..}
          = object
              (catMaybes
                 [("statusDetails" .=) <$> _snpjeStatusDetails])

instance ToPath StartNextPendingJobExecution where
        toPath StartNextPendingJobExecution'{..}
          = mconcat
              ["/things/", toBS _snpjeThingName, "/jobs/$next"]

instance ToQuery StartNextPendingJobExecution where
        toQuery = const mempty

-- | /See:/ 'startNextPendingJobExecutionResponse' smart constructor.
data StartNextPendingJobExecutionResponse = StartNextPendingJobExecutionResponse'
  { _snpjersExecution      :: !(Maybe JobExecution)
  , _snpjersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartNextPendingJobExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snpjersExecution' - A JobExecution object.
--
-- * 'snpjersResponseStatus' - -- | The response status code.
startNextPendingJobExecutionResponse
    :: Int -- ^ 'snpjersResponseStatus'
    -> StartNextPendingJobExecutionResponse
startNextPendingJobExecutionResponse pResponseStatus_ =
  StartNextPendingJobExecutionResponse'
    {_snpjersExecution = Nothing, _snpjersResponseStatus = pResponseStatus_}


-- | A JobExecution object.
snpjersExecution :: Lens' StartNextPendingJobExecutionResponse (Maybe JobExecution)
snpjersExecution = lens _snpjersExecution (\ s a -> s{_snpjersExecution = a})

-- | -- | The response status code.
snpjersResponseStatus :: Lens' StartNextPendingJobExecutionResponse Int
snpjersResponseStatus = lens _snpjersResponseStatus (\ s a -> s{_snpjersResponseStatus = a})

instance NFData StartNextPendingJobExecutionResponse
         where

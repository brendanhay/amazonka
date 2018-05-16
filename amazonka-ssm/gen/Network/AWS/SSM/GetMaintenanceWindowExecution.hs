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
-- Module      : Network.AWS.SSM.GetMaintenanceWindowExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a specific task executed as part of a Maintenance Window execution.
--
--
module Network.AWS.SSM.GetMaintenanceWindowExecution
    (
    -- * Creating a Request
      getMaintenanceWindowExecution
    , GetMaintenanceWindowExecution
    -- * Request Lenses
    , gmweWindowExecutionId

    -- * Destructuring the Response
    , getMaintenanceWindowExecutionResponse
    , GetMaintenanceWindowExecutionResponse
    -- * Response Lenses
    , gmwersStatus
    , gmwersStartTime
    , gmwersWindowExecutionId
    , gmwersStatusDetails
    , gmwersEndTime
    , gmwersTaskIds
    , gmwersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getMaintenanceWindowExecution' smart constructor.
newtype GetMaintenanceWindowExecution = GetMaintenanceWindowExecution'
  { _gmweWindowExecutionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmweWindowExecutionId' - The ID of the Maintenance Window execution that includes the task.
getMaintenanceWindowExecution
    :: Text -- ^ 'gmweWindowExecutionId'
    -> GetMaintenanceWindowExecution
getMaintenanceWindowExecution pWindowExecutionId_ =
  GetMaintenanceWindowExecution' {_gmweWindowExecutionId = pWindowExecutionId_}


-- | The ID of the Maintenance Window execution that includes the task.
gmweWindowExecutionId :: Lens' GetMaintenanceWindowExecution Text
gmweWindowExecutionId = lens _gmweWindowExecutionId (\ s a -> s{_gmweWindowExecutionId = a})

instance AWSRequest GetMaintenanceWindowExecution
         where
        type Rs GetMaintenanceWindowExecution =
             GetMaintenanceWindowExecutionResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetMaintenanceWindowExecutionResponse' <$>
                   (x .?> "Status") <*> (x .?> "StartTime") <*>
                     (x .?> "WindowExecutionId")
                     <*> (x .?> "StatusDetails")
                     <*> (x .?> "EndTime")
                     <*> (x .?> "TaskIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetMaintenanceWindowExecution where

instance NFData GetMaintenanceWindowExecution where

instance ToHeaders GetMaintenanceWindowExecution
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetMaintenanceWindowExecution" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMaintenanceWindowExecution where
        toJSON GetMaintenanceWindowExecution'{..}
          = object
              (catMaybes
                 [Just
                    ("WindowExecutionId" .= _gmweWindowExecutionId)])

instance ToPath GetMaintenanceWindowExecution where
        toPath = const "/"

instance ToQuery GetMaintenanceWindowExecution where
        toQuery = const mempty

-- | /See:/ 'getMaintenanceWindowExecutionResponse' smart constructor.
data GetMaintenanceWindowExecutionResponse = GetMaintenanceWindowExecutionResponse'
  { _gmwersStatus            :: !(Maybe MaintenanceWindowExecutionStatus)
  , _gmwersStartTime         :: !(Maybe POSIX)
  , _gmwersWindowExecutionId :: !(Maybe Text)
  , _gmwersStatusDetails     :: !(Maybe Text)
  , _gmwersEndTime           :: !(Maybe POSIX)
  , _gmwersTaskIds           :: !(Maybe [Text])
  , _gmwersResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMaintenanceWindowExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmwersStatus' - The status of the Maintenance Window execution.
--
-- * 'gmwersStartTime' - The time the Maintenance Window started executing.
--
-- * 'gmwersWindowExecutionId' - The ID of the Maintenance Window execution.
--
-- * 'gmwersStatusDetails' - The details explaining the Status. Only available for certain status values.
--
-- * 'gmwersEndTime' - The time the Maintenance Window finished executing.
--
-- * 'gmwersTaskIds' - The ID of the task executions from the Maintenance Window execution.
--
-- * 'gmwersResponseStatus' - -- | The response status code.
getMaintenanceWindowExecutionResponse
    :: Int -- ^ 'gmwersResponseStatus'
    -> GetMaintenanceWindowExecutionResponse
getMaintenanceWindowExecutionResponse pResponseStatus_ =
  GetMaintenanceWindowExecutionResponse'
    { _gmwersStatus = Nothing
    , _gmwersStartTime = Nothing
    , _gmwersWindowExecutionId = Nothing
    , _gmwersStatusDetails = Nothing
    , _gmwersEndTime = Nothing
    , _gmwersTaskIds = Nothing
    , _gmwersResponseStatus = pResponseStatus_
    }


-- | The status of the Maintenance Window execution.
gmwersStatus :: Lens' GetMaintenanceWindowExecutionResponse (Maybe MaintenanceWindowExecutionStatus)
gmwersStatus = lens _gmwersStatus (\ s a -> s{_gmwersStatus = a})

-- | The time the Maintenance Window started executing.
gmwersStartTime :: Lens' GetMaintenanceWindowExecutionResponse (Maybe UTCTime)
gmwersStartTime = lens _gmwersStartTime (\ s a -> s{_gmwersStartTime = a}) . mapping _Time

-- | The ID of the Maintenance Window execution.
gmwersWindowExecutionId :: Lens' GetMaintenanceWindowExecutionResponse (Maybe Text)
gmwersWindowExecutionId = lens _gmwersWindowExecutionId (\ s a -> s{_gmwersWindowExecutionId = a})

-- | The details explaining the Status. Only available for certain status values.
gmwersStatusDetails :: Lens' GetMaintenanceWindowExecutionResponse (Maybe Text)
gmwersStatusDetails = lens _gmwersStatusDetails (\ s a -> s{_gmwersStatusDetails = a})

-- | The time the Maintenance Window finished executing.
gmwersEndTime :: Lens' GetMaintenanceWindowExecutionResponse (Maybe UTCTime)
gmwersEndTime = lens _gmwersEndTime (\ s a -> s{_gmwersEndTime = a}) . mapping _Time

-- | The ID of the task executions from the Maintenance Window execution.
gmwersTaskIds :: Lens' GetMaintenanceWindowExecutionResponse [Text]
gmwersTaskIds = lens _gmwersTaskIds (\ s a -> s{_gmwersTaskIds = a}) . _Default . _Coerce

-- | -- | The response status code.
gmwersResponseStatus :: Lens' GetMaintenanceWindowExecutionResponse Int
gmwersResponseStatus = lens _gmwersResponseStatus (\ s a -> s{_gmwersResponseStatus = a})

instance NFData GetMaintenanceWindowExecutionResponse
         where

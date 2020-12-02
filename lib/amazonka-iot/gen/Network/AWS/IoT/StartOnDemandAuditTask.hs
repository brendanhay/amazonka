{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StartOnDemandAuditTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand Device Defender audit.
module Network.AWS.IoT.StartOnDemandAuditTask
  ( -- * Creating a Request
    startOnDemandAuditTask,
    StartOnDemandAuditTask,

    -- * Request Lenses
    sodatTargetCheckNames,

    -- * Destructuring the Response
    startOnDemandAuditTaskResponse,
    StartOnDemandAuditTaskResponse,

    -- * Response Lenses
    sodatrsTaskId,
    sodatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startOnDemandAuditTask' smart constructor.
newtype StartOnDemandAuditTask = StartOnDemandAuditTask'
  { _sodatTargetCheckNames ::
      [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartOnDemandAuditTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sodatTargetCheckNames' - Which checks are performed during the audit. The checks you specify must be enabled for your account or an exception occurs. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or @UpdateAccountAuditConfiguration@ to select which checks are enabled.
startOnDemandAuditTask ::
  StartOnDemandAuditTask
startOnDemandAuditTask =
  StartOnDemandAuditTask' {_sodatTargetCheckNames = mempty}

-- | Which checks are performed during the audit. The checks you specify must be enabled for your account or an exception occurs. Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or @UpdateAccountAuditConfiguration@ to select which checks are enabled.
sodatTargetCheckNames :: Lens' StartOnDemandAuditTask [Text]
sodatTargetCheckNames = lens _sodatTargetCheckNames (\s a -> s {_sodatTargetCheckNames = a}) . _Coerce

instance AWSRequest StartOnDemandAuditTask where
  type Rs StartOnDemandAuditTask = StartOnDemandAuditTaskResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          StartOnDemandAuditTaskResponse'
            <$> (x .?> "taskId") <*> (pure (fromEnum s))
      )

instance Hashable StartOnDemandAuditTask

instance NFData StartOnDemandAuditTask

instance ToHeaders StartOnDemandAuditTask where
  toHeaders = const mempty

instance ToJSON StartOnDemandAuditTask where
  toJSON StartOnDemandAuditTask' {..} =
    object
      (catMaybes [Just ("targetCheckNames" .= _sodatTargetCheckNames)])

instance ToPath StartOnDemandAuditTask where
  toPath = const "/audit/tasks"

instance ToQuery StartOnDemandAuditTask where
  toQuery = const mempty

-- | /See:/ 'startOnDemandAuditTaskResponse' smart constructor.
data StartOnDemandAuditTaskResponse = StartOnDemandAuditTaskResponse'
  { _sodatrsTaskId ::
      !(Maybe Text),
    _sodatrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartOnDemandAuditTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sodatrsTaskId' - The ID of the on-demand audit you started.
--
-- * 'sodatrsResponseStatus' - -- | The response status code.
startOnDemandAuditTaskResponse ::
  -- | 'sodatrsResponseStatus'
  Int ->
  StartOnDemandAuditTaskResponse
startOnDemandAuditTaskResponse pResponseStatus_ =
  StartOnDemandAuditTaskResponse'
    { _sodatrsTaskId = Nothing,
      _sodatrsResponseStatus = pResponseStatus_
    }

-- | The ID of the on-demand audit you started.
sodatrsTaskId :: Lens' StartOnDemandAuditTaskResponse (Maybe Text)
sodatrsTaskId = lens _sodatrsTaskId (\s a -> s {_sodatrsTaskId = a})

-- | -- | The response status code.
sodatrsResponseStatus :: Lens' StartOnDemandAuditTaskResponse Int
sodatrsResponseStatus = lens _sodatrsResponseStatus (\s a -> s {_sodatrsResponseStatus = a})

instance NFData StartOnDemandAuditTaskResponse

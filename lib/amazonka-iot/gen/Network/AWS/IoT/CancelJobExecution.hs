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
-- Module      : Network.AWS.IoT.CancelJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the execution of a job for a given thing.
module Network.AWS.IoT.CancelJobExecution
  ( -- * Creating a Request
    cancelJobExecution,
    CancelJobExecution,

    -- * Request Lenses
    cjeForce,
    cjeStatusDetails,
    cjeExpectedVersion,
    cjeJobId,
    cjeThingName,

    -- * Destructuring the Response
    cancelJobExecutionResponse,
    CancelJobExecutionResponse,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelJobExecution' smart constructor.
data CancelJobExecution = CancelJobExecution'
  { _cjeForce ::
      !(Maybe Bool),
    _cjeStatusDetails :: !(Maybe (Map Text (Text))),
    _cjeExpectedVersion :: !(Maybe Integer),
    _cjeJobId :: !Text,
    _cjeThingName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelJobExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjeForce' - (Optional) If @true@ the job execution will be canceled if it has status IN_PROGRESS or QUEUED, otherwise the job execution will be canceled only if it has status QUEUED. If you attempt to cancel a job execution that is IN_PROGRESS, and you do not set @force@ to @true@ , then an @InvalidStateTransitionException@ will be thrown. The default is @false@ . Canceling a job execution which is "IN_PROGRESS", will cause the device to be unable to update the job execution status. Use caution and ensure that the device is able to recover to a valid state.
--
-- * 'cjeStatusDetails' - A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged. You can specify at most 10 name/value pairs.
--
-- * 'cjeExpectedVersion' - (Optional) The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
--
-- * 'cjeJobId' - The ID of the job to be canceled.
--
-- * 'cjeThingName' - The name of the thing whose execution of the job will be canceled.
cancelJobExecution ::
  -- | 'cjeJobId'
  Text ->
  -- | 'cjeThingName'
  Text ->
  CancelJobExecution
cancelJobExecution pJobId_ pThingName_ =
  CancelJobExecution'
    { _cjeForce = Nothing,
      _cjeStatusDetails = Nothing,
      _cjeExpectedVersion = Nothing,
      _cjeJobId = pJobId_,
      _cjeThingName = pThingName_
    }

-- | (Optional) If @true@ the job execution will be canceled if it has status IN_PROGRESS or QUEUED, otherwise the job execution will be canceled only if it has status QUEUED. If you attempt to cancel a job execution that is IN_PROGRESS, and you do not set @force@ to @true@ , then an @InvalidStateTransitionException@ will be thrown. The default is @false@ . Canceling a job execution which is "IN_PROGRESS", will cause the device to be unable to update the job execution status. Use caution and ensure that the device is able to recover to a valid state.
cjeForce :: Lens' CancelJobExecution (Maybe Bool)
cjeForce = lens _cjeForce (\s a -> s {_cjeForce = a})

-- | A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged. You can specify at most 10 name/value pairs.
cjeStatusDetails :: Lens' CancelJobExecution (HashMap Text (Text))
cjeStatusDetails = lens _cjeStatusDetails (\s a -> s {_cjeStatusDetails = a}) . _Default . _Map

-- | (Optional) The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
cjeExpectedVersion :: Lens' CancelJobExecution (Maybe Integer)
cjeExpectedVersion = lens _cjeExpectedVersion (\s a -> s {_cjeExpectedVersion = a})

-- | The ID of the job to be canceled.
cjeJobId :: Lens' CancelJobExecution Text
cjeJobId = lens _cjeJobId (\s a -> s {_cjeJobId = a})

-- | The name of the thing whose execution of the job will be canceled.
cjeThingName :: Lens' CancelJobExecution Text
cjeThingName = lens _cjeThingName (\s a -> s {_cjeThingName = a})

instance AWSRequest CancelJobExecution where
  type Rs CancelJobExecution = CancelJobExecutionResponse
  request = putJSON ioT
  response = receiveNull CancelJobExecutionResponse'

instance Hashable CancelJobExecution

instance NFData CancelJobExecution

instance ToHeaders CancelJobExecution where
  toHeaders = const mempty

instance ToJSON CancelJobExecution where
  toJSON CancelJobExecution' {..} =
    object
      ( catMaybes
          [ ("statusDetails" .=) <$> _cjeStatusDetails,
            ("expectedVersion" .=) <$> _cjeExpectedVersion
          ]
      )

instance ToPath CancelJobExecution where
  toPath CancelJobExecution' {..} =
    mconcat
      [ "/things/",
        toBS _cjeThingName,
        "/jobs/",
        toBS _cjeJobId,
        "/cancel"
      ]

instance ToQuery CancelJobExecution where
  toQuery CancelJobExecution' {..} = mconcat ["force" =: _cjeForce]

-- | /See:/ 'cancelJobExecutionResponse' smart constructor.
data CancelJobExecutionResponse = CancelJobExecutionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelJobExecutionResponse' with the minimum fields required to make a request.
cancelJobExecutionResponse ::
  CancelJobExecutionResponse
cancelJobExecutionResponse = CancelJobExecutionResponse'

instance NFData CancelJobExecutionResponse

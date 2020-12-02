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
-- Module      : Network.AWS.SageMaker.CreateMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a schedule that regularly starts Amazon SageMaker Processing Jobs to monitor the data captured for an Amazon SageMaker Endoint.
module Network.AWS.SageMaker.CreateMonitoringSchedule
  ( -- * Creating a Request
    createMonitoringSchedule,
    CreateMonitoringSchedule,

    -- * Request Lenses
    cmsTags,
    cmsMonitoringScheduleName,
    cmsMonitoringScheduleConfig,

    -- * Destructuring the Response
    createMonitoringScheduleResponse,
    CreateMonitoringScheduleResponse,

    -- * Response Lenses
    cmsrsResponseStatus,
    cmsrsMonitoringScheduleARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createMonitoringSchedule' smart constructor.
data CreateMonitoringSchedule = CreateMonitoringSchedule'
  { _cmsTags ::
      !(Maybe [Tag]),
    _cmsMonitoringScheduleName :: !Text,
    _cmsMonitoringScheduleConfig ::
      !MonitoringScheduleConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMonitoringSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmsTags' - (Optional) An array of key-value pairs. For more information, see < https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'cmsMonitoringScheduleName' - The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
--
-- * 'cmsMonitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and defines the monitoring job.
createMonitoringSchedule ::
  -- | 'cmsMonitoringScheduleName'
  Text ->
  -- | 'cmsMonitoringScheduleConfig'
  MonitoringScheduleConfig ->
  CreateMonitoringSchedule
createMonitoringSchedule
  pMonitoringScheduleName_
  pMonitoringScheduleConfig_ =
    CreateMonitoringSchedule'
      { _cmsTags = Nothing,
        _cmsMonitoringScheduleName = pMonitoringScheduleName_,
        _cmsMonitoringScheduleConfig = pMonitoringScheduleConfig_
      }

-- | (Optional) An array of key-value pairs. For more information, see < https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
cmsTags :: Lens' CreateMonitoringSchedule [Tag]
cmsTags = lens _cmsTags (\s a -> s {_cmsTags = a}) . _Default . _Coerce

-- | The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
cmsMonitoringScheduleName :: Lens' CreateMonitoringSchedule Text
cmsMonitoringScheduleName = lens _cmsMonitoringScheduleName (\s a -> s {_cmsMonitoringScheduleName = a})

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
cmsMonitoringScheduleConfig :: Lens' CreateMonitoringSchedule MonitoringScheduleConfig
cmsMonitoringScheduleConfig = lens _cmsMonitoringScheduleConfig (\s a -> s {_cmsMonitoringScheduleConfig = a})

instance AWSRequest CreateMonitoringSchedule where
  type Rs CreateMonitoringSchedule = CreateMonitoringScheduleResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateMonitoringScheduleResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "MonitoringScheduleArn")
      )

instance Hashable CreateMonitoringSchedule

instance NFData CreateMonitoringSchedule

instance ToHeaders CreateMonitoringSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.CreateMonitoringSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateMonitoringSchedule where
  toJSON CreateMonitoringSchedule' {..} =
    object
      ( catMaybes
          [ ("Tags" .=) <$> _cmsTags,
            Just ("MonitoringScheduleName" .= _cmsMonitoringScheduleName),
            Just ("MonitoringScheduleConfig" .= _cmsMonitoringScheduleConfig)
          ]
      )

instance ToPath CreateMonitoringSchedule where
  toPath = const "/"

instance ToQuery CreateMonitoringSchedule where
  toQuery = const mempty

-- | /See:/ 'createMonitoringScheduleResponse' smart constructor.
data CreateMonitoringScheduleResponse = CreateMonitoringScheduleResponse'
  { _cmsrsResponseStatus ::
      !Int,
    _cmsrsMonitoringScheduleARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMonitoringScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmsrsResponseStatus' - -- | The response status code.
--
-- * 'cmsrsMonitoringScheduleARN' - The Amazon Resource Name (ARN) of the monitoring schedule.
createMonitoringScheduleResponse ::
  -- | 'cmsrsResponseStatus'
  Int ->
  -- | 'cmsrsMonitoringScheduleARN'
  Text ->
  CreateMonitoringScheduleResponse
createMonitoringScheduleResponse
  pResponseStatus_
  pMonitoringScheduleARN_ =
    CreateMonitoringScheduleResponse'
      { _cmsrsResponseStatus =
          pResponseStatus_,
        _cmsrsMonitoringScheduleARN = pMonitoringScheduleARN_
      }

-- | -- | The response status code.
cmsrsResponseStatus :: Lens' CreateMonitoringScheduleResponse Int
cmsrsResponseStatus = lens _cmsrsResponseStatus (\s a -> s {_cmsrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
cmsrsMonitoringScheduleARN :: Lens' CreateMonitoringScheduleResponse Text
cmsrsMonitoringScheduleARN = lens _cmsrsMonitoringScheduleARN (\s a -> s {_cmsrsMonitoringScheduleARN = a})

instance NFData CreateMonitoringScheduleResponse

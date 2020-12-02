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
-- Module      : Network.AWS.IoT.CreateScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled audit that is run at a specified time interval.
module Network.AWS.IoT.CreateScheduledAudit
  ( -- * Creating a Request
    createScheduledAudit,
    CreateScheduledAudit,

    -- * Request Lenses
    csaDayOfMonth,
    csaDayOfWeek,
    csaTags,
    csaFrequency,
    csaTargetCheckNames,
    csaScheduledAuditName,

    -- * Destructuring the Response
    createScheduledAuditResponse,
    CreateScheduledAuditResponse,

    -- * Response Lenses
    csarsScheduledAuditARN,
    csarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createScheduledAudit' smart constructor.
data CreateScheduledAudit = CreateScheduledAudit'
  { _csaDayOfMonth ::
      !(Maybe Text),
    _csaDayOfWeek :: !(Maybe DayOfWeek),
    _csaTags :: !(Maybe [Tag]),
    _csaFrequency :: !AuditFrequency,
    _csaTargetCheckNames :: ![Text],
    _csaScheduledAuditName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateScheduledAudit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csaDayOfMonth' - The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- * 'csaDayOfWeek' - The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
--
-- * 'csaTags' - Metadata that can be used to manage the scheduled audit.
--
-- * 'csaFrequency' - How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY" or "MONTHLY". The start time of each audit is determined by the system.
--
-- * 'csaTargetCheckNames' - Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- * 'csaScheduledAuditName' - The name you want to give to the scheduled audit. (Max. 128 chars)
createScheduledAudit ::
  -- | 'csaFrequency'
  AuditFrequency ->
  -- | 'csaScheduledAuditName'
  Text ->
  CreateScheduledAudit
createScheduledAudit pFrequency_ pScheduledAuditName_ =
  CreateScheduledAudit'
    { _csaDayOfMonth = Nothing,
      _csaDayOfWeek = Nothing,
      _csaTags = Nothing,
      _csaFrequency = pFrequency_,
      _csaTargetCheckNames = mempty,
      _csaScheduledAuditName = pScheduledAuditName_
    }

-- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
csaDayOfMonth :: Lens' CreateScheduledAudit (Maybe Text)
csaDayOfMonth = lens _csaDayOfMonth (\s a -> s {_csaDayOfMonth = a})

-- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
csaDayOfWeek :: Lens' CreateScheduledAudit (Maybe DayOfWeek)
csaDayOfWeek = lens _csaDayOfWeek (\s a -> s {_csaDayOfWeek = a})

-- | Metadata that can be used to manage the scheduled audit.
csaTags :: Lens' CreateScheduledAudit [Tag]
csaTags = lens _csaTags (\s a -> s {_csaTags = a}) . _Default . _Coerce

-- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY" or "MONTHLY". The start time of each audit is determined by the system.
csaFrequency :: Lens' CreateScheduledAudit AuditFrequency
csaFrequency = lens _csaFrequency (\s a -> s {_csaFrequency = a})

-- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
csaTargetCheckNames :: Lens' CreateScheduledAudit [Text]
csaTargetCheckNames = lens _csaTargetCheckNames (\s a -> s {_csaTargetCheckNames = a}) . _Coerce

-- | The name you want to give to the scheduled audit. (Max. 128 chars)
csaScheduledAuditName :: Lens' CreateScheduledAudit Text
csaScheduledAuditName = lens _csaScheduledAuditName (\s a -> s {_csaScheduledAuditName = a})

instance AWSRequest CreateScheduledAudit where
  type Rs CreateScheduledAudit = CreateScheduledAuditResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateScheduledAuditResponse'
            <$> (x .?> "scheduledAuditArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateScheduledAudit

instance NFData CreateScheduledAudit

instance ToHeaders CreateScheduledAudit where
  toHeaders = const mempty

instance ToJSON CreateScheduledAudit where
  toJSON CreateScheduledAudit' {..} =
    object
      ( catMaybes
          [ ("dayOfMonth" .=) <$> _csaDayOfMonth,
            ("dayOfWeek" .=) <$> _csaDayOfWeek,
            ("tags" .=) <$> _csaTags,
            Just ("frequency" .= _csaFrequency),
            Just ("targetCheckNames" .= _csaTargetCheckNames)
          ]
      )

instance ToPath CreateScheduledAudit where
  toPath CreateScheduledAudit' {..} =
    mconcat ["/audit/scheduledaudits/", toBS _csaScheduledAuditName]

instance ToQuery CreateScheduledAudit where
  toQuery = const mempty

-- | /See:/ 'createScheduledAuditResponse' smart constructor.
data CreateScheduledAuditResponse = CreateScheduledAuditResponse'
  { _csarsScheduledAuditARN ::
      !(Maybe Text),
    _csarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateScheduledAuditResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csarsScheduledAuditARN' - The ARN of the scheduled audit.
--
-- * 'csarsResponseStatus' - -- | The response status code.
createScheduledAuditResponse ::
  -- | 'csarsResponseStatus'
  Int ->
  CreateScheduledAuditResponse
createScheduledAuditResponse pResponseStatus_ =
  CreateScheduledAuditResponse'
    { _csarsScheduledAuditARN = Nothing,
      _csarsResponseStatus = pResponseStatus_
    }

-- | The ARN of the scheduled audit.
csarsScheduledAuditARN :: Lens' CreateScheduledAuditResponse (Maybe Text)
csarsScheduledAuditARN = lens _csarsScheduledAuditARN (\s a -> s {_csarsScheduledAuditARN = a})

-- | -- | The response status code.
csarsResponseStatus :: Lens' CreateScheduledAuditResponse Int
csarsResponseStatus = lens _csarsResponseStatus (\s a -> s {_csarsResponseStatus = a})

instance NFData CreateScheduledAuditResponse

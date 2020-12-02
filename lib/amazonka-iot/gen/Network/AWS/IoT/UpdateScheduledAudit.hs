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
-- Module      : Network.AWS.IoT.UpdateScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a scheduled audit, including which checks are performed and how often the audit takes place.
module Network.AWS.IoT.UpdateScheduledAudit
  ( -- * Creating a Request
    updateScheduledAudit,
    UpdateScheduledAudit,

    -- * Request Lenses
    usaFrequency,
    usaDayOfMonth,
    usaTargetCheckNames,
    usaDayOfWeek,
    usaScheduledAuditName,

    -- * Destructuring the Response
    updateScheduledAuditResponse,
    UpdateScheduledAuditResponse,

    -- * Response Lenses
    usarsScheduledAuditARN,
    usarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateScheduledAudit' smart constructor.
data UpdateScheduledAudit = UpdateScheduledAudit'
  { _usaFrequency ::
      !(Maybe AuditFrequency),
    _usaDayOfMonth :: !(Maybe Text),
    _usaTargetCheckNames :: !(Maybe [Text]),
    _usaDayOfWeek :: !(Maybe DayOfWeek),
    _usaScheduledAuditName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateScheduledAudit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usaFrequency' - How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
--
-- * 'usaDayOfMonth' - The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- * 'usaTargetCheckNames' - Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- * 'usaDayOfWeek' - The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
--
-- * 'usaScheduledAuditName' - The name of the scheduled audit. (Max. 128 chars)
updateScheduledAudit ::
  -- | 'usaScheduledAuditName'
  Text ->
  UpdateScheduledAudit
updateScheduledAudit pScheduledAuditName_ =
  UpdateScheduledAudit'
    { _usaFrequency = Nothing,
      _usaDayOfMonth = Nothing,
      _usaTargetCheckNames = Nothing,
      _usaDayOfWeek = Nothing,
      _usaScheduledAuditName = pScheduledAuditName_
    }

-- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
usaFrequency :: Lens' UpdateScheduledAudit (Maybe AuditFrequency)
usaFrequency = lens _usaFrequency (\s a -> s {_usaFrequency = a})

-- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
usaDayOfMonth :: Lens' UpdateScheduledAudit (Maybe Text)
usaDayOfMonth = lens _usaDayOfMonth (\s a -> s {_usaDayOfMonth = a})

-- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
usaTargetCheckNames :: Lens' UpdateScheduledAudit [Text]
usaTargetCheckNames = lens _usaTargetCheckNames (\s a -> s {_usaTargetCheckNames = a}) . _Default . _Coerce

-- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
usaDayOfWeek :: Lens' UpdateScheduledAudit (Maybe DayOfWeek)
usaDayOfWeek = lens _usaDayOfWeek (\s a -> s {_usaDayOfWeek = a})

-- | The name of the scheduled audit. (Max. 128 chars)
usaScheduledAuditName :: Lens' UpdateScheduledAudit Text
usaScheduledAuditName = lens _usaScheduledAuditName (\s a -> s {_usaScheduledAuditName = a})

instance AWSRequest UpdateScheduledAudit where
  type Rs UpdateScheduledAudit = UpdateScheduledAuditResponse
  request = patchJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          UpdateScheduledAuditResponse'
            <$> (x .?> "scheduledAuditArn") <*> (pure (fromEnum s))
      )

instance Hashable UpdateScheduledAudit

instance NFData UpdateScheduledAudit

instance ToHeaders UpdateScheduledAudit where
  toHeaders = const mempty

instance ToJSON UpdateScheduledAudit where
  toJSON UpdateScheduledAudit' {..} =
    object
      ( catMaybes
          [ ("frequency" .=) <$> _usaFrequency,
            ("dayOfMonth" .=) <$> _usaDayOfMonth,
            ("targetCheckNames" .=) <$> _usaTargetCheckNames,
            ("dayOfWeek" .=) <$> _usaDayOfWeek
          ]
      )

instance ToPath UpdateScheduledAudit where
  toPath UpdateScheduledAudit' {..} =
    mconcat ["/audit/scheduledaudits/", toBS _usaScheduledAuditName]

instance ToQuery UpdateScheduledAudit where
  toQuery = const mempty

-- | /See:/ 'updateScheduledAuditResponse' smart constructor.
data UpdateScheduledAuditResponse = UpdateScheduledAuditResponse'
  { _usarsScheduledAuditARN ::
      !(Maybe Text),
    _usarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateScheduledAuditResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usarsScheduledAuditARN' - The ARN of the scheduled audit.
--
-- * 'usarsResponseStatus' - -- | The response status code.
updateScheduledAuditResponse ::
  -- | 'usarsResponseStatus'
  Int ->
  UpdateScheduledAuditResponse
updateScheduledAuditResponse pResponseStatus_ =
  UpdateScheduledAuditResponse'
    { _usarsScheduledAuditARN = Nothing,
      _usarsResponseStatus = pResponseStatus_
    }

-- | The ARN of the scheduled audit.
usarsScheduledAuditARN :: Lens' UpdateScheduledAuditResponse (Maybe Text)
usarsScheduledAuditARN = lens _usarsScheduledAuditARN (\s a -> s {_usarsScheduledAuditARN = a})

-- | -- | The response status code.
usarsResponseStatus :: Lens' UpdateScheduledAuditResponse Int
usarsResponseStatus = lens _usarsResponseStatus (\s a -> s {_usarsResponseStatus = a})

instance NFData UpdateScheduledAuditResponse

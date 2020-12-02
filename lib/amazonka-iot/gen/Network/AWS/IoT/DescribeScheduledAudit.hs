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
-- Module      : Network.AWS.IoT.DescribeScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a scheduled audit.
module Network.AWS.IoT.DescribeScheduledAudit
  ( -- * Creating a Request
    describeScheduledAudit,
    DescribeScheduledAudit,

    -- * Request Lenses
    dScheduledAuditName,

    -- * Destructuring the Response
    describeScheduledAuditResponse,
    DescribeScheduledAuditResponse,

    -- * Response Lenses
    dsarsFrequency,
    dsarsScheduledAuditName,
    dsarsDayOfMonth,
    dsarsTargetCheckNames,
    dsarsDayOfWeek,
    dsarsScheduledAuditARN,
    dsarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeScheduledAudit' smart constructor.
newtype DescribeScheduledAudit = DescribeScheduledAudit'
  { _dScheduledAuditName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeScheduledAudit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dScheduledAuditName' - The name of the scheduled audit whose information you want to get.
describeScheduledAudit ::
  -- | 'dScheduledAuditName'
  Text ->
  DescribeScheduledAudit
describeScheduledAudit pScheduledAuditName_ =
  DescribeScheduledAudit'
    { _dScheduledAuditName =
        pScheduledAuditName_
    }

-- | The name of the scheduled audit whose information you want to get.
dScheduledAuditName :: Lens' DescribeScheduledAudit Text
dScheduledAuditName = lens _dScheduledAuditName (\s a -> s {_dScheduledAuditName = a})

instance AWSRequest DescribeScheduledAudit where
  type Rs DescribeScheduledAudit = DescribeScheduledAuditResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeScheduledAuditResponse'
            <$> (x .?> "frequency")
            <*> (x .?> "scheduledAuditName")
            <*> (x .?> "dayOfMonth")
            <*> (x .?> "targetCheckNames" .!@ mempty)
            <*> (x .?> "dayOfWeek")
            <*> (x .?> "scheduledAuditArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeScheduledAudit

instance NFData DescribeScheduledAudit

instance ToHeaders DescribeScheduledAudit where
  toHeaders = const mempty

instance ToPath DescribeScheduledAudit where
  toPath DescribeScheduledAudit' {..} =
    mconcat ["/audit/scheduledaudits/", toBS _dScheduledAuditName]

instance ToQuery DescribeScheduledAudit where
  toQuery = const mempty

-- | /See:/ 'describeScheduledAuditResponse' smart constructor.
data DescribeScheduledAuditResponse = DescribeScheduledAuditResponse'
  { _dsarsFrequency ::
      !(Maybe AuditFrequency),
    _dsarsScheduledAuditName ::
      !(Maybe Text),
    _dsarsDayOfMonth ::
      !(Maybe Text),
    _dsarsTargetCheckNames ::
      !(Maybe [Text]),
    _dsarsDayOfWeek ::
      !(Maybe DayOfWeek),
    _dsarsScheduledAuditARN ::
      !(Maybe Text),
    _dsarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeScheduledAuditResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarsFrequency' - How often the scheduled audit takes place. One of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
--
-- * 'dsarsScheduledAuditName' - The name of the scheduled audit.
--
-- * 'dsarsDayOfMonth' - The day of the month on which the scheduled audit takes place. Will be "1" through "31" or "LAST". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- * 'dsarsTargetCheckNames' - Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- * 'dsarsDayOfWeek' - The day of the week on which the scheduled audit takes place. One of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT".
--
-- * 'dsarsScheduledAuditARN' - The ARN of the scheduled audit.
--
-- * 'dsarsResponseStatus' - -- | The response status code.
describeScheduledAuditResponse ::
  -- | 'dsarsResponseStatus'
  Int ->
  DescribeScheduledAuditResponse
describeScheduledAuditResponse pResponseStatus_ =
  DescribeScheduledAuditResponse'
    { _dsarsFrequency = Nothing,
      _dsarsScheduledAuditName = Nothing,
      _dsarsDayOfMonth = Nothing,
      _dsarsTargetCheckNames = Nothing,
      _dsarsDayOfWeek = Nothing,
      _dsarsScheduledAuditARN = Nothing,
      _dsarsResponseStatus = pResponseStatus_
    }

-- | How often the scheduled audit takes place. One of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
dsarsFrequency :: Lens' DescribeScheduledAuditResponse (Maybe AuditFrequency)
dsarsFrequency = lens _dsarsFrequency (\s a -> s {_dsarsFrequency = a})

-- | The name of the scheduled audit.
dsarsScheduledAuditName :: Lens' DescribeScheduledAuditResponse (Maybe Text)
dsarsScheduledAuditName = lens _dsarsScheduledAuditName (\s a -> s {_dsarsScheduledAuditName = a})

-- | The day of the month on which the scheduled audit takes place. Will be "1" through "31" or "LAST". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
dsarsDayOfMonth :: Lens' DescribeScheduledAuditResponse (Maybe Text)
dsarsDayOfMonth = lens _dsarsDayOfMonth (\s a -> s {_dsarsDayOfMonth = a})

-- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
dsarsTargetCheckNames :: Lens' DescribeScheduledAuditResponse [Text]
dsarsTargetCheckNames = lens _dsarsTargetCheckNames (\s a -> s {_dsarsTargetCheckNames = a}) . _Default . _Coerce

-- | The day of the week on which the scheduled audit takes place. One of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT".
dsarsDayOfWeek :: Lens' DescribeScheduledAuditResponse (Maybe DayOfWeek)
dsarsDayOfWeek = lens _dsarsDayOfWeek (\s a -> s {_dsarsDayOfWeek = a})

-- | The ARN of the scheduled audit.
dsarsScheduledAuditARN :: Lens' DescribeScheduledAuditResponse (Maybe Text)
dsarsScheduledAuditARN = lens _dsarsScheduledAuditARN (\s a -> s {_dsarsScheduledAuditARN = a})

-- | -- | The response status code.
dsarsResponseStatus :: Lens' DescribeScheduledAuditResponse Int
dsarsResponseStatus = lens _dsarsResponseStatus (\s a -> s {_dsarsResponseStatus = a})

instance NFData DescribeScheduledAuditResponse

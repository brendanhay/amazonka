{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSession where

import Network.AWS.DeviceFarm.Types.TestGridSessionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A 'TestGridSession' is a single instance of a browser launched from the URL provided by a call to 'CreateTestGridUrl' .
--
--
--
-- /See:/ 'testGridSession' smart constructor.
data TestGridSession = TestGridSession'
  { _tgsStatus ::
      !(Maybe TestGridSessionStatus),
    _tgsArn :: !(Maybe Text),
    _tgsCreated :: !(Maybe POSIX),
    _tgsBillingMinutes :: !(Maybe Double),
    _tgsEnded :: !(Maybe POSIX),
    _tgsSeleniumProperties :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestGridSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgsStatus' - The state of the session.
--
-- * 'tgsArn' - The ARN of the session.
--
-- * 'tgsCreated' - The time that the session was started.
--
-- * 'tgsBillingMinutes' - The number of billed minutes that were used for this session.
--
-- * 'tgsEnded' - The time the session ended.
--
-- * 'tgsSeleniumProperties' - A JSON object of options and parameters passed to the Selenium WebDriver.
testGridSession ::
  TestGridSession
testGridSession =
  TestGridSession'
    { _tgsStatus = Nothing,
      _tgsArn = Nothing,
      _tgsCreated = Nothing,
      _tgsBillingMinutes = Nothing,
      _tgsEnded = Nothing,
      _tgsSeleniumProperties = Nothing
    }

-- | The state of the session.
tgsStatus :: Lens' TestGridSession (Maybe TestGridSessionStatus)
tgsStatus = lens _tgsStatus (\s a -> s {_tgsStatus = a})

-- | The ARN of the session.
tgsArn :: Lens' TestGridSession (Maybe Text)
tgsArn = lens _tgsArn (\s a -> s {_tgsArn = a})

-- | The time that the session was started.
tgsCreated :: Lens' TestGridSession (Maybe UTCTime)
tgsCreated = lens _tgsCreated (\s a -> s {_tgsCreated = a}) . mapping _Time

-- | The number of billed minutes that were used for this session.
tgsBillingMinutes :: Lens' TestGridSession (Maybe Double)
tgsBillingMinutes = lens _tgsBillingMinutes (\s a -> s {_tgsBillingMinutes = a})

-- | The time the session ended.
tgsEnded :: Lens' TestGridSession (Maybe UTCTime)
tgsEnded = lens _tgsEnded (\s a -> s {_tgsEnded = a}) . mapping _Time

-- | A JSON object of options and parameters passed to the Selenium WebDriver.
tgsSeleniumProperties :: Lens' TestGridSession (Maybe Text)
tgsSeleniumProperties = lens _tgsSeleniumProperties (\s a -> s {_tgsSeleniumProperties = a})

instance FromJSON TestGridSession where
  parseJSON =
    withObject
      "TestGridSession"
      ( \x ->
          TestGridSession'
            <$> (x .:? "status")
            <*> (x .:? "arn")
            <*> (x .:? "created")
            <*> (x .:? "billingMinutes")
            <*> (x .:? "ended")
            <*> (x .:? "seleniumProperties")
      )

instance Hashable TestGridSession

instance NFData TestGridSession

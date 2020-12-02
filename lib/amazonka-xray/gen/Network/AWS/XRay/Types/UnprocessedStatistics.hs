{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.UnprocessedStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.UnprocessedStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Sampling statistics from a call to 'GetSamplingTargets' that X-Ray could not process.
--
--
--
-- /See:/ 'unprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { _usRuleName ::
      !(Maybe Text),
    _usErrorCode :: !(Maybe Text),
    _usMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnprocessedStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usRuleName' - The name of the sampling rule.
--
-- * 'usErrorCode' - The error code.
--
-- * 'usMessage' - The error message.
unprocessedStatistics ::
  UnprocessedStatistics
unprocessedStatistics =
  UnprocessedStatistics'
    { _usRuleName = Nothing,
      _usErrorCode = Nothing,
      _usMessage = Nothing
    }

-- | The name of the sampling rule.
usRuleName :: Lens' UnprocessedStatistics (Maybe Text)
usRuleName = lens _usRuleName (\s a -> s {_usRuleName = a})

-- | The error code.
usErrorCode :: Lens' UnprocessedStatistics (Maybe Text)
usErrorCode = lens _usErrorCode (\s a -> s {_usErrorCode = a})

-- | The error message.
usMessage :: Lens' UnprocessedStatistics (Maybe Text)
usMessage = lens _usMessage (\s a -> s {_usMessage = a})

instance FromJSON UnprocessedStatistics where
  parseJSON =
    withObject
      "UnprocessedStatistics"
      ( \x ->
          UnprocessedStatistics'
            <$> (x .:? "RuleName") <*> (x .:? "ErrorCode") <*> (x .:? "Message")
      )

instance Hashable UnprocessedStatistics

instance NFData UnprocessedStatistics

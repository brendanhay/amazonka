{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Concurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Concurrency where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'concurrency' smart constructor.
newtype Concurrency = Concurrency'
  { _cReservedConcurrentExecutions ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Concurrency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cReservedConcurrentExecutions' - The number of concurrent executions that are reserved for this function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
concurrency ::
  Concurrency
concurrency =
  Concurrency' {_cReservedConcurrentExecutions = Nothing}

-- | The number of concurrent executions that are reserved for this function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
cReservedConcurrentExecutions :: Lens' Concurrency (Maybe Natural)
cReservedConcurrentExecutions = lens _cReservedConcurrentExecutions (\s a -> s {_cReservedConcurrentExecutions = a}) . mapping _Nat

instance FromJSON Concurrency where
  parseJSON =
    withObject
      "Concurrency"
      (\x -> Concurrency' <$> (x .:? "ReservedConcurrentExecutions"))

instance Hashable Concurrency

instance NFData Concurrency

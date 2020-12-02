{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageResourceResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageResourceResult where

import Network.AWS.GuardDuty.Types.Total
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the sum of usage based on an AWS resource.
--
--
--
-- /See:/ 'usageResourceResult' smart constructor.
data UsageResourceResult = UsageResourceResult'
  { _urrTotal ::
      !(Maybe Total),
    _urrResource :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageResourceResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrTotal' - Represents the sum total of usage for the specified resource type.
--
-- * 'urrResource' - The AWS resource that generated usage.
usageResourceResult ::
  UsageResourceResult
usageResourceResult =
  UsageResourceResult' {_urrTotal = Nothing, _urrResource = Nothing}

-- | Represents the sum total of usage for the specified resource type.
urrTotal :: Lens' UsageResourceResult (Maybe Total)
urrTotal = lens _urrTotal (\s a -> s {_urrTotal = a})

-- | The AWS resource that generated usage.
urrResource :: Lens' UsageResourceResult (Maybe Text)
urrResource = lens _urrResource (\s a -> s {_urrResource = a})

instance FromJSON UsageResourceResult where
  parseJSON =
    withObject
      "UsageResourceResult"
      ( \x ->
          UsageResourceResult' <$> (x .:? "total") <*> (x .:? "resource")
      )

instance Hashable UsageResourceResult

instance NFData UsageResourceResult

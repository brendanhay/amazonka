{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RootCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RootCause where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The combination of AWS service, linked account, Region, and usage type where a cost anomaly is observed.
--
--
--
-- /See:/ 'rootCause' smart constructor.
data RootCause = RootCause'
  { _rcService :: !(Maybe Text),
    _rcUsageType :: !(Maybe Text),
    _rcLinkedAccount :: !(Maybe Text),
    _rcRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RootCause' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcService' - The AWS service name associated with the cost anomaly.
--
-- * 'rcUsageType' - The @UsageType@ value associated with the cost anomaly.
--
-- * 'rcLinkedAccount' - The linked account value associated with the cost anomaly.
--
-- * 'rcRegion' - The AWS Region associated with the cost anomaly.
rootCause ::
  RootCause
rootCause =
  RootCause'
    { _rcService = Nothing,
      _rcUsageType = Nothing,
      _rcLinkedAccount = Nothing,
      _rcRegion = Nothing
    }

-- | The AWS service name associated with the cost anomaly.
rcService :: Lens' RootCause (Maybe Text)
rcService = lens _rcService (\s a -> s {_rcService = a})

-- | The @UsageType@ value associated with the cost anomaly.
rcUsageType :: Lens' RootCause (Maybe Text)
rcUsageType = lens _rcUsageType (\s a -> s {_rcUsageType = a})

-- | The linked account value associated with the cost anomaly.
rcLinkedAccount :: Lens' RootCause (Maybe Text)
rcLinkedAccount = lens _rcLinkedAccount (\s a -> s {_rcLinkedAccount = a})

-- | The AWS Region associated with the cost anomaly.
rcRegion :: Lens' RootCause (Maybe Text)
rcRegion = lens _rcRegion (\s a -> s {_rcRegion = a})

instance FromJSON RootCause where
  parseJSON =
    withObject
      "RootCause"
      ( \x ->
          RootCause'
            <$> (x .:? "Service")
            <*> (x .:? "UsageType")
            <*> (x .:? "LinkedAccount")
            <*> (x .:? "Region")
      )

instance Hashable RootCause

instance NFData RootCause

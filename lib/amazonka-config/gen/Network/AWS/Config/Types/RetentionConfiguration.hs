{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RetentionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RetentionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object with the name of the retention configuration and the retention period in days. The object stores the configuration for data retention in AWS Config.
--
--
--
-- /See:/ 'retentionConfiguration' smart constructor.
data RetentionConfiguration = RetentionConfiguration'
  { _rcName ::
      !Text,
    _rcRetentionPeriodInDays :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RetentionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcName' - The name of the retention configuration object.
--
-- * 'rcRetentionPeriodInDays' - Number of days AWS Config stores your historical information.
retentionConfiguration ::
  -- | 'rcName'
  Text ->
  -- | 'rcRetentionPeriodInDays'
  Natural ->
  RetentionConfiguration
retentionConfiguration pName_ pRetentionPeriodInDays_ =
  RetentionConfiguration'
    { _rcName = pName_,
      _rcRetentionPeriodInDays = _Nat # pRetentionPeriodInDays_
    }

-- | The name of the retention configuration object.
rcName :: Lens' RetentionConfiguration Text
rcName = lens _rcName (\s a -> s {_rcName = a})

-- | Number of days AWS Config stores your historical information.
rcRetentionPeriodInDays :: Lens' RetentionConfiguration Natural
rcRetentionPeriodInDays = lens _rcRetentionPeriodInDays (\s a -> s {_rcRetentionPeriodInDays = a}) . _Nat

instance FromJSON RetentionConfiguration where
  parseJSON =
    withObject
      "RetentionConfiguration"
      ( \x ->
          RetentionConfiguration'
            <$> (x .: "Name") <*> (x .: "RetentionPeriodInDays")
      )

instance Hashable RetentionConfiguration

instance NFData RetentionConfiguration

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.S3LogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3LogsConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes whether S3 data event logs will be enabled as a data source.
--
--
--
-- /See:/ 's3LogsConfiguration' smart constructor.
newtype S3LogsConfiguration = S3LogsConfiguration'
  { _slcEnable ::
      Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3LogsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcEnable' - The status of S3 data event logs as a data source.
s3LogsConfiguration ::
  -- | 'slcEnable'
  Bool ->
  S3LogsConfiguration
s3LogsConfiguration pEnable_ =
  S3LogsConfiguration' {_slcEnable = pEnable_}

-- | The status of S3 data event logs as a data source.
slcEnable :: Lens' S3LogsConfiguration Bool
slcEnable = lens _slcEnable (\s a -> s {_slcEnable = a})

instance Hashable S3LogsConfiguration

instance NFData S3LogsConfiguration

instance ToJSON S3LogsConfiguration where
  toJSON S3LogsConfiguration' {..} =
    object (catMaybes [Just ("enable" .= _slcEnable)])

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketLoggingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketLoggingStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LoggingEnabled

-- | Container for logging status information.
--
--
--
-- /See:/ 'bucketLoggingStatus' smart constructor.
newtype BucketLoggingStatus = BucketLoggingStatus'
  { _blsLoggingEnabled ::
      Maybe LoggingEnabled
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BucketLoggingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blsLoggingEnabled' - Undocumented member.
bucketLoggingStatus ::
  BucketLoggingStatus
bucketLoggingStatus =
  BucketLoggingStatus' {_blsLoggingEnabled = Nothing}

-- | Undocumented member.
blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled = lens _blsLoggingEnabled (\s a -> s {_blsLoggingEnabled = a})

instance Hashable BucketLoggingStatus

instance NFData BucketLoggingStatus

instance ToXML BucketLoggingStatus where
  toXML BucketLoggingStatus' {..} =
    mconcat ["LoggingEnabled" @= _blsLoggingEnabled]

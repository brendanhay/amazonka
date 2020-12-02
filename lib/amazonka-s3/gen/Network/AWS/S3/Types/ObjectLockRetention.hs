{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockRetention where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockRetentionMode

-- | A Retention configuration for an object.
--
--
--
-- /See:/ 'objectLockRetention' smart constructor.
data ObjectLockRetention = ObjectLockRetention'
  { _olrMode ::
      !(Maybe ObjectLockRetentionMode),
    _olrRetainUntilDate :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectLockRetention' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olrMode' - Indicates the Retention mode for the specified object.
--
-- * 'olrRetainUntilDate' - The date on which this Object Lock Retention will expire.
objectLockRetention ::
  ObjectLockRetention
objectLockRetention =
  ObjectLockRetention'
    { _olrMode = Nothing,
      _olrRetainUntilDate = Nothing
    }

-- | Indicates the Retention mode for the specified object.
olrMode :: Lens' ObjectLockRetention (Maybe ObjectLockRetentionMode)
olrMode = lens _olrMode (\s a -> s {_olrMode = a})

-- | The date on which this Object Lock Retention will expire.
olrRetainUntilDate :: Lens' ObjectLockRetention (Maybe UTCTime)
olrRetainUntilDate = lens _olrRetainUntilDate (\s a -> s {_olrRetainUntilDate = a}) . mapping _Time

instance FromXML ObjectLockRetention where
  parseXML x =
    ObjectLockRetention'
      <$> (x .@? "Mode") <*> (x .@? "RetainUntilDate")

instance Hashable ObjectLockRetention

instance NFData ObjectLockRetention

instance ToXML ObjectLockRetention where
  toXML ObjectLockRetention' {..} =
    mconcat
      ["Mode" @= _olrMode, "RetainUntilDate" @= _olrRetainUntilDate]

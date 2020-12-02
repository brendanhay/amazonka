{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockLegalHold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockLegalHold where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockLegalHoldStatus

-- | A Legal Hold configuration for an object.
--
--
--
-- /See:/ 'objectLockLegalHold' smart constructor.
newtype ObjectLockLegalHold = ObjectLockLegalHold'
  { _ollhStatus ::
      Maybe ObjectLockLegalHoldStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectLockLegalHold' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ollhStatus' - Indicates whether the specified object has a Legal Hold in place.
objectLockLegalHold ::
  ObjectLockLegalHold
objectLockLegalHold = ObjectLockLegalHold' {_ollhStatus = Nothing}

-- | Indicates whether the specified object has a Legal Hold in place.
ollhStatus :: Lens' ObjectLockLegalHold (Maybe ObjectLockLegalHoldStatus)
ollhStatus = lens _ollhStatus (\s a -> s {_ollhStatus = a})

instance FromXML ObjectLockLegalHold where
  parseXML x = ObjectLockLegalHold' <$> (x .@? "Status")

instance Hashable ObjectLockLegalHold

instance NFData ObjectLockLegalHold

instance ToXML ObjectLockLegalHold where
  toXML ObjectLockLegalHold' {..} = mconcat ["Status" @= _ollhStatus]

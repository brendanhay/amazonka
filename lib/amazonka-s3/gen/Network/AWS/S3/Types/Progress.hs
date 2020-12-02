{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Progress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Progress where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | This data type contains information about progress of an operation.
--
--
--
-- /See:/ 'progress' smart constructor.
data Progress = Progress'
  { _pBytesReturned :: !(Maybe Integer),
    _pBytesScanned :: !(Maybe Integer),
    _pBytesProcessed :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Progress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pBytesReturned' - The current number of bytes of records payload data returned.
--
-- * 'pBytesScanned' - The current number of object bytes scanned.
--
-- * 'pBytesProcessed' - The current number of uncompressed object bytes processed.
progress ::
  Progress
progress =
  Progress'
    { _pBytesReturned = Nothing,
      _pBytesScanned = Nothing,
      _pBytesProcessed = Nothing
    }

-- | The current number of bytes of records payload data returned.
pBytesReturned :: Lens' Progress (Maybe Integer)
pBytesReturned = lens _pBytesReturned (\s a -> s {_pBytesReturned = a})

-- | The current number of object bytes scanned.
pBytesScanned :: Lens' Progress (Maybe Integer)
pBytesScanned = lens _pBytesScanned (\s a -> s {_pBytesScanned = a})

-- | The current number of uncompressed object bytes processed.
pBytesProcessed :: Lens' Progress (Maybe Integer)
pBytesProcessed = lens _pBytesProcessed (\s a -> s {_pBytesProcessed = a})

instance FromXML Progress where
  parseXML x =
    Progress'
      <$> (x .@? "BytesReturned")
      <*> (x .@? "BytesScanned")
      <*> (x .@? "BytesProcessed")

instance Hashable Progress

instance NFData Progress

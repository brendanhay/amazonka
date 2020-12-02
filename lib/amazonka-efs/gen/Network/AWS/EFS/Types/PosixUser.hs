{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.PosixUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.PosixUser where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The full POSIX identity, including the user ID, group ID, and any secondary group IDs, on the access point that is used for all file system operations performed by NFS clients using the access point.
--
--
--
-- /See:/ 'posixUser' smart constructor.
data PosixUser = PosixUser'
  { _puSecondaryGids :: !(Maybe [Nat]),
    _puUid :: !Nat,
    _puGid :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PosixUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puSecondaryGids' - Secondary POSIX group IDs used for all file system operations using this access point.
--
-- * 'puUid' - The POSIX user ID used for all file system operations using this access point.
--
-- * 'puGid' - The POSIX group ID used for all file system operations using this access point.
posixUser ::
  -- | 'puUid'
  Natural ->
  -- | 'puGid'
  Natural ->
  PosixUser
posixUser pUid_ pGid_ =
  PosixUser'
    { _puSecondaryGids = Nothing,
      _puUid = _Nat # pUid_,
      _puGid = _Nat # pGid_
    }

-- | Secondary POSIX group IDs used for all file system operations using this access point.
puSecondaryGids :: Lens' PosixUser [Natural]
puSecondaryGids = lens _puSecondaryGids (\s a -> s {_puSecondaryGids = a}) . _Default . _Coerce

-- | The POSIX user ID used for all file system operations using this access point.
puUid :: Lens' PosixUser Natural
puUid = lens _puUid (\s a -> s {_puUid = a}) . _Nat

-- | The POSIX group ID used for all file system operations using this access point.
puGid :: Lens' PosixUser Natural
puGid = lens _puGid (\s a -> s {_puGid = a}) . _Nat

instance FromJSON PosixUser where
  parseJSON =
    withObject
      "PosixUser"
      ( \x ->
          PosixUser'
            <$> (x .:? "SecondaryGids" .!= mempty)
            <*> (x .: "Uid")
            <*> (x .: "Gid")
      )

instance Hashable PosixUser

instance NFData PosixUser

instance ToJSON PosixUser where
  toJSON PosixUser' {..} =
    object
      ( catMaybes
          [ ("SecondaryGids" .=) <$> _puSecondaryGids,
            Just ("Uid" .= _puUid),
            Just ("Gid" .= _puGid)
          ]
      )

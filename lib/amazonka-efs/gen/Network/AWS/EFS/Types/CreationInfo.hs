{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.CreationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.CreationInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Required if the @RootDirectory@ > @Path@ specified does not exist. Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ > @Path@ . If the access point root directory does not exist, EFS creates it with these settings when a client connects to the access point. When specifying @CreationInfo@ , you must include values for all properties.
--
--
-- /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ does not exist, attempts to mount the file system using the access point will fail.
--
--
-- /See:/ 'creationInfo' smart constructor.
data CreationInfo = CreationInfo'
  { _ciOwnerUid :: !Nat,
    _ciOwnerGid :: !Nat,
    _ciPermissions :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciOwnerUid' - Specifies the POSIX user ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
--
-- * 'ciOwnerGid' - Specifies the POSIX group ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
--
-- * 'ciPermissions' - Specifies the POSIX permissions to apply to the @RootDirectory@ , in the format of an octal number representing the file's mode bits.
creationInfo ::
  -- | 'ciOwnerUid'
  Natural ->
  -- | 'ciOwnerGid'
  Natural ->
  -- | 'ciPermissions'
  Text ->
  CreationInfo
creationInfo pOwnerUid_ pOwnerGid_ pPermissions_ =
  CreationInfo'
    { _ciOwnerUid = _Nat # pOwnerUid_,
      _ciOwnerGid = _Nat # pOwnerGid_,
      _ciPermissions = pPermissions_
    }

-- | Specifies the POSIX user ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
ciOwnerUid :: Lens' CreationInfo Natural
ciOwnerUid = lens _ciOwnerUid (\s a -> s {_ciOwnerUid = a}) . _Nat

-- | Specifies the POSIX group ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
ciOwnerGid :: Lens' CreationInfo Natural
ciOwnerGid = lens _ciOwnerGid (\s a -> s {_ciOwnerGid = a}) . _Nat

-- | Specifies the POSIX permissions to apply to the @RootDirectory@ , in the format of an octal number representing the file's mode bits.
ciPermissions :: Lens' CreationInfo Text
ciPermissions = lens _ciPermissions (\s a -> s {_ciPermissions = a})

instance FromJSON CreationInfo where
  parseJSON =
    withObject
      "CreationInfo"
      ( \x ->
          CreationInfo'
            <$> (x .: "OwnerUid") <*> (x .: "OwnerGid") <*> (x .: "Permissions")
      )

instance Hashable CreationInfo

instance NFData CreationInfo

instance ToJSON CreationInfo where
  toJSON CreationInfo' {..} =
    object
      ( catMaybes
          [ Just ("OwnerUid" .= _ciOwnerUid),
            Just ("OwnerGid" .= _ciOwnerGid),
            Just ("Permissions" .= _ciPermissions)
          ]
      )

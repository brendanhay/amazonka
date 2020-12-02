{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.RootDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.RootDirectory where

import Network.AWS.EFS.Types.CreationInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the directory on the Amazon EFS file system that the access point provides access to. The access point exposes the specified file system path as the root directory of your file system to applications using the access point. NFS clients using the access point can only access data in the access point's @RootDirectory@ and it's subdirectories.
--
--
--
-- /See:/ 'rootDirectory' smart constructor.
data RootDirectory = RootDirectory'
  { _rdCreationInfo ::
      !(Maybe CreationInfo),
    _rdPath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RootDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdCreationInfo' - (Optional) Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ . If the @RootDirectory@ > @Path@ specified does not exist, EFS creates the root directory using the @CreationInfo@ settings when a client connects to an access point. When specifying the @CreationInfo@ , you must provide values for all properties.  /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ > @Path@ does not exist, attempts to mount the file system using the access point will fail.
--
-- * 'rdPath' - Specifies the path on the EFS file system to expose as the root directory to NFS clients using the access point to access the EFS file system. A path can have up to four subdirectories. If the specified path does not exist, you are required to provide the @CreationInfo@ .
rootDirectory ::
  RootDirectory
rootDirectory =
  RootDirectory' {_rdCreationInfo = Nothing, _rdPath = Nothing}

-- | (Optional) Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ . If the @RootDirectory@ > @Path@ specified does not exist, EFS creates the root directory using the @CreationInfo@ settings when a client connects to an access point. When specifying the @CreationInfo@ , you must provide values for all properties.  /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ > @Path@ does not exist, attempts to mount the file system using the access point will fail.
rdCreationInfo :: Lens' RootDirectory (Maybe CreationInfo)
rdCreationInfo = lens _rdCreationInfo (\s a -> s {_rdCreationInfo = a})

-- | Specifies the path on the EFS file system to expose as the root directory to NFS clients using the access point to access the EFS file system. A path can have up to four subdirectories. If the specified path does not exist, you are required to provide the @CreationInfo@ .
rdPath :: Lens' RootDirectory (Maybe Text)
rdPath = lens _rdPath (\s a -> s {_rdPath = a})

instance FromJSON RootDirectory where
  parseJSON =
    withObject
      "RootDirectory"
      ( \x ->
          RootDirectory' <$> (x .:? "CreationInfo") <*> (x .:? "Path")
      )

instance Hashable RootDirectory

instance NFData RootDirectory

instance ToJSON RootDirectory where
  toJSON RootDirectory' {..} =
    object
      ( catMaybes
          [("CreationInfo" .=) <$> _rdCreationInfo, ("Path" .=) <$> _rdPath]
      )

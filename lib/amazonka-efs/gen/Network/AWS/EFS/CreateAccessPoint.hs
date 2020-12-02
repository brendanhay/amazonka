{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateAccessPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EFS access point. An access point is an application-specific view into an EFS file system that applies an operating system user and group, and a file system path, to any file system request made through the access point. The operating system user and group override any identity information provided by the NFS client. The file system path is exposed as the access point's root directory. Applications using the access point can only access data in its own directory and below. To learn more, see <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Mounting a File System Using EFS Access Points> .
--
--
-- This operation requires permissions for the @elasticfilesystem:CreateAccessPoint@ action.
module Network.AWS.EFS.CreateAccessPoint
  ( -- * Creating a Request
    createAccessPoint,
    CreateAccessPoint,

    -- * Request Lenses
    capPosixUser,
    capRootDirectory,
    capTags,
    capClientToken,
    capFileSystemId,

    -- * Destructuring the Response
    accessPointDescription,
    AccessPointDescription,

    -- * Response Lenses
    apdPosixUser,
    apdRootDirectory,
    apdClientToken,
    apdAccessPointId,
    apdFileSystemId,
    apdOwnerId,
    apdName,
    apdAccessPointARN,
    apdLifeCycleState,
    apdTags,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAccessPoint' smart constructor.
data CreateAccessPoint = CreateAccessPoint'
  { _capPosixUser ::
      !(Maybe PosixUser),
    _capRootDirectory :: !(Maybe RootDirectory),
    _capTags :: !(Maybe [Tag]),
    _capClientToken :: !Text,
    _capFileSystemId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAccessPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'capPosixUser' - The operating system user and group applied to all file system requests made using the access point.
--
-- * 'capRootDirectory' - Specifies the directory on the Amazon EFS file system that the access point exposes as the root directory of your file system to NFS clients using the access point. The clients using the access point can only access the root directory and below. If the @RootDirectory@ > @Path@ specified does not exist, EFS creates it and applies the @CreationInfo@ settings when a client connects to an access point. When specifying a @RootDirectory@ , you need to provide the @Path@ , and the @CreationInfo@ is optional.
--
-- * 'capTags' - Creates tags associated with the access point. Each tag is a key-value pair.
--
-- * 'capClientToken' - A string of up to 64 ASCII characters that Amazon EFS uses to ensure idempotent creation.
--
-- * 'capFileSystemId' - The ID of the EFS file system that the access point provides access to.
createAccessPoint ::
  -- | 'capClientToken'
  Text ->
  -- | 'capFileSystemId'
  Text ->
  CreateAccessPoint
createAccessPoint pClientToken_ pFileSystemId_ =
  CreateAccessPoint'
    { _capPosixUser = Nothing,
      _capRootDirectory = Nothing,
      _capTags = Nothing,
      _capClientToken = pClientToken_,
      _capFileSystemId = pFileSystemId_
    }

-- | The operating system user and group applied to all file system requests made using the access point.
capPosixUser :: Lens' CreateAccessPoint (Maybe PosixUser)
capPosixUser = lens _capPosixUser (\s a -> s {_capPosixUser = a})

-- | Specifies the directory on the Amazon EFS file system that the access point exposes as the root directory of your file system to NFS clients using the access point. The clients using the access point can only access the root directory and below. If the @RootDirectory@ > @Path@ specified does not exist, EFS creates it and applies the @CreationInfo@ settings when a client connects to an access point. When specifying a @RootDirectory@ , you need to provide the @Path@ , and the @CreationInfo@ is optional.
capRootDirectory :: Lens' CreateAccessPoint (Maybe RootDirectory)
capRootDirectory = lens _capRootDirectory (\s a -> s {_capRootDirectory = a})

-- | Creates tags associated with the access point. Each tag is a key-value pair.
capTags :: Lens' CreateAccessPoint [Tag]
capTags = lens _capTags (\s a -> s {_capTags = a}) . _Default . _Coerce

-- | A string of up to 64 ASCII characters that Amazon EFS uses to ensure idempotent creation.
capClientToken :: Lens' CreateAccessPoint Text
capClientToken = lens _capClientToken (\s a -> s {_capClientToken = a})

-- | The ID of the EFS file system that the access point provides access to.
capFileSystemId :: Lens' CreateAccessPoint Text
capFileSystemId = lens _capFileSystemId (\s a -> s {_capFileSystemId = a})

instance AWSRequest CreateAccessPoint where
  type Rs CreateAccessPoint = AccessPointDescription
  request = postJSON efs
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable CreateAccessPoint

instance NFData CreateAccessPoint

instance ToHeaders CreateAccessPoint where
  toHeaders = const mempty

instance ToJSON CreateAccessPoint where
  toJSON CreateAccessPoint' {..} =
    object
      ( catMaybes
          [ ("PosixUser" .=) <$> _capPosixUser,
            ("RootDirectory" .=) <$> _capRootDirectory,
            ("Tags" .=) <$> _capTags,
            Just ("ClientToken" .= _capClientToken),
            Just ("FileSystemId" .= _capFileSystemId)
          ]
      )

instance ToPath CreateAccessPoint where
  toPath = const "/2015-02-01/access-points"

instance ToQuery CreateAccessPoint where
  toQuery = const mempty

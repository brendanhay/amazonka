{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateFileSystem
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty file system. The operation requires a creation
-- token in the request that Amazon EFS uses to ensure idempotent creation
-- (calling the operation with same creation token has no effect). If a
-- file system does not currently exist that is owned by the caller\'s AWS
-- account with the specified creation token, this operation does the
-- following:
--
-- -   Creates a new, empty file system. The file system will have an
--     Amazon EFS assigned ID, and an initial lifecycle state \"creating\".
-- -   Returns with the description of the created file system.
--
-- Otherwise, this operation returns a @FileSystemAlreadyExists@ error with
-- the ID of the existing file system.
--
-- For basic use cases, you can use a randomly generated UUID for the
-- creation token.
--
-- The idempotent operation allows you to retry a @CreateFileSystem@ call
-- without risk of creating an extra file system. This can happen when an
-- initial call fails in a way that leaves it uncertain whether or not a
-- file system was actually created. An example might be that a transport
-- level timeout occurred or your connection was reset. As long as you use
-- the same creation token, if the initial call had succeeded in creating a
-- file system, the client can learn of its existence from the
-- @FileSystemAlreadyExists@ error.
--
-- The @CreateFileSystem@ call returns while the file system\'s lifecycle
-- state is still \"creating\". You can check the file system creation
-- status by calling the DescribeFileSystems API, which among other things
-- returns the file system state.
--
-- After the file system is fully created, Amazon EFS sets its lifecycle
-- state to \"available\", at which point you can create one or more mount
-- targets for the file system (CreateMountTarget) in your VPC. You mount
-- your Amazon EFS file system on an EC2 instances in your VPC via the
-- mount target. For more information, see
-- <http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works>
--
-- This operation requires permission for the
-- @elasticfilesystem:CreateFileSystem@ action.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateFileSystem.html>
module Network.AWS.EFS.CreateFileSystem
    (
    -- * Request
      CreateFileSystem
    -- ** Request constructor
    , createFileSystem
    -- ** Request lenses
    , cfsCreationToken

    -- * Response
    , FileSystemDescription
    -- ** Response constructor
    , fileSystemDescription
    -- ** Response lenses
    , fsdName
    , fsdOwnerId
    , fsdCreationToken
    , fsdFileSystemId
    , fsdCreationTime
    , fsdLifeCycleState
    , fsdNumberOfMountTargets
    , fsdSizeInBytes
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createFileSystem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfsCreationToken'
newtype CreateFileSystem = CreateFileSystem'
    { _cfsCreationToken :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateFileSystem' smart constructor.
createFileSystem :: Text -> CreateFileSystem
createFileSystem pCreationToken =
    CreateFileSystem'
    { _cfsCreationToken = pCreationToken
    }

-- | String of up to 64 ASCII characters. Amazon EFS uses this to ensure
-- idempotent creation.
cfsCreationToken :: Lens' CreateFileSystem Text
cfsCreationToken = lens _cfsCreationToken (\ s a -> s{_cfsCreationToken = a});

instance AWSRequest CreateFileSystem where
        type Sv CreateFileSystem = EFS
        type Rs CreateFileSystem = FileSystemDescription
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateFileSystem where
        toHeaders = const mempty

instance ToJSON CreateFileSystem where
        toJSON CreateFileSystem'{..}
          = object ["CreationToken" .= _cfsCreationToken]

instance ToPath CreateFileSystem where
        toPath = const "/2015-02-01/file-systems"

instance ToQuery CreateFileSystem where
        toQuery = const mempty

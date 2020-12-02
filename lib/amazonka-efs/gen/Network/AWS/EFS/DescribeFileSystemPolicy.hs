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
-- Module      : Network.AWS.EFS.DescribeFileSystemPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @FileSystemPolicy@ for the specified EFS file system.
--
--
-- This operation requires permissions for the @elasticfilesystem:DescribeFileSystemPolicy@ action.
module Network.AWS.EFS.DescribeFileSystemPolicy
  ( -- * Creating a Request
    describeFileSystemPolicy,
    DescribeFileSystemPolicy,

    -- * Request Lenses
    desFileSystemId,

    -- * Destructuring the Response
    fileSystemPolicyDescription,
    FileSystemPolicyDescription,

    -- * Response Lenses
    fspdFileSystemId,
    fspdPolicy,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFileSystemPolicy' smart constructor.
newtype DescribeFileSystemPolicy = DescribeFileSystemPolicy'
  { _desFileSystemId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFileSystemPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desFileSystemId' - Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
describeFileSystemPolicy ::
  -- | 'desFileSystemId'
  Text ->
  DescribeFileSystemPolicy
describeFileSystemPolicy pFileSystemId_ =
  DescribeFileSystemPolicy' {_desFileSystemId = pFileSystemId_}

-- | Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
desFileSystemId :: Lens' DescribeFileSystemPolicy Text
desFileSystemId = lens _desFileSystemId (\s a -> s {_desFileSystemId = a})

instance AWSRequest DescribeFileSystemPolicy where
  type Rs DescribeFileSystemPolicy = FileSystemPolicyDescription
  request = get efs
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable DescribeFileSystemPolicy

instance NFData DescribeFileSystemPolicy

instance ToHeaders DescribeFileSystemPolicy where
  toHeaders = const mempty

instance ToPath DescribeFileSystemPolicy where
  toPath DescribeFileSystemPolicy' {..} =
    mconcat
      ["/2015-02-01/file-systems/", toBS _desFileSystemId, "/policy"]

instance ToQuery DescribeFileSystemPolicy where
  toQuery = const mempty

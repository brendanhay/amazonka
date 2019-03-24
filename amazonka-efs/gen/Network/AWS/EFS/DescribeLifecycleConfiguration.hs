{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeLifecycleConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current @LifecycleConfiguration@ object for the specified Amazon EFS file system. EFS lifecycle management uses the @LifecycleConfiguration@ object to identify which files to move to the EFS Infrequent Access (IA) storage class. For a file system without a @LifecycleConfiguration@ object, the call returns an empty array in the response.
--
--
-- This operation requires permissions for the @elasticfilesystem:DescribeLifecycleConfiguration@ operation.
--
module Network.AWS.EFS.DescribeLifecycleConfiguration
    (
    -- * Creating a Request
      describeLifecycleConfiguration
    , DescribeLifecycleConfiguration
    -- * Request Lenses
    , dlcFileSystemId

    -- * Destructuring the Response
    , lifecycleConfigurationDescription
    , LifecycleConfigurationDescription
    -- * Response Lenses
    , lcdLifecyclePolicies
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLifecycleConfiguration' smart constructor.
newtype DescribeLifecycleConfiguration = DescribeLifecycleConfiguration'
  { _dlcFileSystemId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcFileSystemId' - The ID of the file system whose @LifecycleConfiguration@ object you want to retrieve (String).
describeLifecycleConfiguration
    :: Text -- ^ 'dlcFileSystemId'
    -> DescribeLifecycleConfiguration
describeLifecycleConfiguration pFileSystemId_ =
  DescribeLifecycleConfiguration' {_dlcFileSystemId = pFileSystemId_}


-- | The ID of the file system whose @LifecycleConfiguration@ object you want to retrieve (String).
dlcFileSystemId :: Lens' DescribeLifecycleConfiguration Text
dlcFileSystemId = lens _dlcFileSystemId (\ s a -> s{_dlcFileSystemId = a})

instance AWSRequest DescribeLifecycleConfiguration
         where
        type Rs DescribeLifecycleConfiguration =
             LifecycleConfigurationDescription
        request = get efs
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DescribeLifecycleConfiguration
         where

instance NFData DescribeLifecycleConfiguration where

instance ToHeaders DescribeLifecycleConfiguration
         where
        toHeaders = const mempty

instance ToPath DescribeLifecycleConfiguration where
        toPath DescribeLifecycleConfiguration'{..}
          = mconcat
              ["/2015-02-01/file-systems/", toBS _dlcFileSystemId,
               "/lifecycle-configuration"]

instance ToQuery DescribeLifecycleConfiguration where
        toQuery = const mempty

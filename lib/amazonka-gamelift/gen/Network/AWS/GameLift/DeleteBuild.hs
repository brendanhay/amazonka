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
-- Module      : Network.AWS.GameLift.DeleteBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a build. This operation permanently deletes the build resource and any uploaded build files. Deleting a build does not affect the status of any active fleets using the build, but you can no longer create new fleets with the deleted build.
--
--
-- To delete a build, specify the build ID.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
--
-- __Related operations__
--
--     * 'CreateBuild'
--
--     * 'ListBuilds'
--
--     * 'DescribeBuild'
--
--     * 'UpdateBuild'
--
--     * 'DeleteBuild'
module Network.AWS.GameLift.DeleteBuild
  ( -- * Creating a Request
    deleteBuild,
    DeleteBuild,

    -- * Request Lenses
    dbBuildId,

    -- * Destructuring the Response
    deleteBuildResponse,
    DeleteBuildResponse,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'deleteBuild' smart constructor.
newtype DeleteBuild = DeleteBuild' {_dbBuildId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbBuildId' - A unique identifier for a build to delete. You can use either the build ID or ARN value.
deleteBuild ::
  -- | 'dbBuildId'
  Text ->
  DeleteBuild
deleteBuild pBuildId_ = DeleteBuild' {_dbBuildId = pBuildId_}

-- | A unique identifier for a build to delete. You can use either the build ID or ARN value.
dbBuildId :: Lens' DeleteBuild Text
dbBuildId = lens _dbBuildId (\s a -> s {_dbBuildId = a})

instance AWSRequest DeleteBuild where
  type Rs DeleteBuild = DeleteBuildResponse
  request = postJSON gameLift
  response = receiveNull DeleteBuildResponse'

instance Hashable DeleteBuild

instance NFData DeleteBuild

instance ToHeaders DeleteBuild where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.DeleteBuild" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteBuild where
  toJSON DeleteBuild' {..} =
    object (catMaybes [Just ("BuildId" .= _dbBuildId)])

instance ToPath DeleteBuild where
  toPath = const "/"

instance ToQuery DeleteBuild where
  toQuery = const mempty

-- | /See:/ 'deleteBuildResponse' smart constructor.
data DeleteBuildResponse = DeleteBuildResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBuildResponse' with the minimum fields required to make a request.
deleteBuildResponse ::
  DeleteBuildResponse
deleteBuildResponse = DeleteBuildResponse'

instance NFData DeleteBuildResponse

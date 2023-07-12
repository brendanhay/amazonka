{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.DeleteBuild
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a build. This operation permanently deletes the build resource
-- and any uploaded build files. Deleting a build does not affect the
-- status of any active fleets using the build, but you can no longer
-- create new fleets with the deleted build.
--
-- To delete a build, specify the build ID.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DeleteBuild
  ( -- * Creating a Request
    DeleteBuild (..),
    newDeleteBuild,

    -- * Request Lenses
    deleteBuild_buildId,

    -- * Destructuring the Response
    DeleteBuildResponse (..),
    newDeleteBuildResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBuild' smart constructor.
data DeleteBuild = DeleteBuild'
  { -- | A unique identifier for the build to delete. You can use either the
    -- build ID or ARN value.
    buildId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildId', 'deleteBuild_buildId' - A unique identifier for the build to delete. You can use either the
-- build ID or ARN value.
newDeleteBuild ::
  -- | 'buildId'
  Prelude.Text ->
  DeleteBuild
newDeleteBuild pBuildId_ =
  DeleteBuild' {buildId = pBuildId_}

-- | A unique identifier for the build to delete. You can use either the
-- build ID or ARN value.
deleteBuild_buildId :: Lens.Lens' DeleteBuild Prelude.Text
deleteBuild_buildId = Lens.lens (\DeleteBuild' {buildId} -> buildId) (\s@DeleteBuild' {} a -> s {buildId = a} :: DeleteBuild)

instance Core.AWSRequest DeleteBuild where
  type AWSResponse DeleteBuild = DeleteBuildResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteBuildResponse'

instance Prelude.Hashable DeleteBuild where
  hashWithSalt _salt DeleteBuild' {..} =
    _salt `Prelude.hashWithSalt` buildId

instance Prelude.NFData DeleteBuild where
  rnf DeleteBuild' {..} = Prelude.rnf buildId

instance Data.ToHeaders DeleteBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DeleteBuild" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBuild where
  toJSON DeleteBuild' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BuildId" Data..= buildId)]
      )

instance Data.ToPath DeleteBuild where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBuild where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBuildResponse' smart constructor.
data DeleteBuildResponse = DeleteBuildResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBuildResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBuildResponse ::
  DeleteBuildResponse
newDeleteBuildResponse = DeleteBuildResponse'

instance Prelude.NFData DeleteBuildResponse where
  rnf _ = ()

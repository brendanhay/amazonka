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
-- Module      : Amazonka.EFS.DeleteReplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing replication configuration. To delete a replication
-- configuration, you must make the request from the Amazon Web Services
-- Region in which the destination file system is located. Deleting a
-- replication configuration ends the replication process. After a
-- replication configuration is deleted, the destination file system is no
-- longer read-only. You can write to the destination file system after its
-- status becomes @Writeable@.
module Amazonka.EFS.DeleteReplicationConfiguration
  ( -- * Creating a Request
    DeleteReplicationConfiguration (..),
    newDeleteReplicationConfiguration,

    -- * Request Lenses
    deleteReplicationConfiguration_sourceFileSystemId,

    -- * Destructuring the Response
    DeleteReplicationConfigurationResponse (..),
    newDeleteReplicationConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteReplicationConfiguration' smart constructor.
data DeleteReplicationConfiguration = DeleteReplicationConfiguration'
  { -- | The ID of the source file system in the replication configuration.
    sourceFileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFileSystemId', 'deleteReplicationConfiguration_sourceFileSystemId' - The ID of the source file system in the replication configuration.
newDeleteReplicationConfiguration ::
  -- | 'sourceFileSystemId'
  Prelude.Text ->
  DeleteReplicationConfiguration
newDeleteReplicationConfiguration
  pSourceFileSystemId_ =
    DeleteReplicationConfiguration'
      { sourceFileSystemId =
          pSourceFileSystemId_
      }

-- | The ID of the source file system in the replication configuration.
deleteReplicationConfiguration_sourceFileSystemId :: Lens.Lens' DeleteReplicationConfiguration Prelude.Text
deleteReplicationConfiguration_sourceFileSystemId = Lens.lens (\DeleteReplicationConfiguration' {sourceFileSystemId} -> sourceFileSystemId) (\s@DeleteReplicationConfiguration' {} a -> s {sourceFileSystemId = a} :: DeleteReplicationConfiguration)

instance
  Core.AWSRequest
    DeleteReplicationConfiguration
  where
  type
    AWSResponse DeleteReplicationConfiguration =
      DeleteReplicationConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteReplicationConfigurationResponse'

instance
  Prelude.Hashable
    DeleteReplicationConfiguration
  where
  hashWithSalt
    _salt
    DeleteReplicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sourceFileSystemId

instance
  Prelude.NFData
    DeleteReplicationConfiguration
  where
  rnf DeleteReplicationConfiguration' {..} =
    Prelude.rnf sourceFileSystemId

instance
  Core.ToHeaders
    DeleteReplicationConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteReplicationConfiguration where
  toPath DeleteReplicationConfiguration' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Core.toBS sourceFileSystemId,
        "/replication-configuration"
      ]

instance Core.ToQuery DeleteReplicationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteReplicationConfigurationResponse' smart constructor.
data DeleteReplicationConfigurationResponse = DeleteReplicationConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteReplicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteReplicationConfigurationResponse ::
  DeleteReplicationConfigurationResponse
newDeleteReplicationConfigurationResponse =
  DeleteReplicationConfigurationResponse'

instance
  Prelude.NFData
    DeleteReplicationConfigurationResponse
  where
  rnf _ = ()

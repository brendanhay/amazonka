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
-- Module      : Network.AWS.RDS.DeleteInstallationMedia
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the installation medium for a DB engine that requires an
-- on-premises customer provided license, such as Microsoft SQL Server.
module Network.AWS.RDS.DeleteInstallationMedia
  ( -- * Creating a Request
    DeleteInstallationMedia (..),
    newDeleteInstallationMedia,

    -- * Request Lenses
    deleteInstallationMedia_installationMediaId,

    -- * Destructuring the Response
    InstallationMedia (..),
    newInstallationMedia,

    -- * Response Lenses
    installationMedia_status,
    installationMedia_customAvailabilityZoneId,
    installationMedia_installationMediaId,
    installationMedia_engineVersion,
    installationMedia_failureCause,
    installationMedia_oSInstallationMediaPath,
    installationMedia_engine,
    installationMedia_engineInstallationMediaPath,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstallationMedia' smart constructor.
data DeleteInstallationMedia = DeleteInstallationMedia'
  { -- | The installation medium ID.
    installationMediaId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstallationMedia' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'installationMediaId', 'deleteInstallationMedia_installationMediaId' - The installation medium ID.
newDeleteInstallationMedia ::
  -- | 'installationMediaId'
  Prelude.Text ->
  DeleteInstallationMedia
newDeleteInstallationMedia pInstallationMediaId_ =
  DeleteInstallationMedia'
    { installationMediaId =
        pInstallationMediaId_
    }

-- | The installation medium ID.
deleteInstallationMedia_installationMediaId :: Lens.Lens' DeleteInstallationMedia Prelude.Text
deleteInstallationMedia_installationMediaId = Lens.lens (\DeleteInstallationMedia' {installationMediaId} -> installationMediaId) (\s@DeleteInstallationMedia' {} a -> s {installationMediaId = a} :: DeleteInstallationMedia)

instance Core.AWSRequest DeleteInstallationMedia where
  type
    AWSResponse DeleteInstallationMedia =
      InstallationMedia
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteInstallationMediaResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable DeleteInstallationMedia

instance Prelude.NFData DeleteInstallationMedia

instance Core.ToHeaders DeleteInstallationMedia where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteInstallationMedia where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteInstallationMedia where
  toQuery DeleteInstallationMedia' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteInstallationMedia" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "InstallationMediaId" Core.=: installationMediaId
      ]

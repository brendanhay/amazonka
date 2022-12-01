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
-- Module      : Amazonka.AppConfig.DeleteHostedConfigurationVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of a configuration from the AppConfig hosted
-- configuration store.
module Amazonka.AppConfig.DeleteHostedConfigurationVersion
  ( -- * Creating a Request
    DeleteHostedConfigurationVersion (..),
    newDeleteHostedConfigurationVersion,

    -- * Request Lenses
    deleteHostedConfigurationVersion_applicationId,
    deleteHostedConfigurationVersion_configurationProfileId,
    deleteHostedConfigurationVersion_versionNumber,

    -- * Destructuring the Response
    DeleteHostedConfigurationVersionResponse (..),
    newDeleteHostedConfigurationVersionResponse,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteHostedConfigurationVersion' smart constructor.
data DeleteHostedConfigurationVersion = DeleteHostedConfigurationVersion'
  { -- | The application ID.
    applicationId :: Prelude.Text,
    -- | The configuration profile ID.
    configurationProfileId :: Prelude.Text,
    -- | The versions number to delete.
    versionNumber :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHostedConfigurationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteHostedConfigurationVersion_applicationId' - The application ID.
--
-- 'configurationProfileId', 'deleteHostedConfigurationVersion_configurationProfileId' - The configuration profile ID.
--
-- 'versionNumber', 'deleteHostedConfigurationVersion_versionNumber' - The versions number to delete.
newDeleteHostedConfigurationVersion ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'configurationProfileId'
  Prelude.Text ->
  -- | 'versionNumber'
  Prelude.Int ->
  DeleteHostedConfigurationVersion
newDeleteHostedConfigurationVersion
  pApplicationId_
  pConfigurationProfileId_
  pVersionNumber_ =
    DeleteHostedConfigurationVersion'
      { applicationId =
          pApplicationId_,
        configurationProfileId =
          pConfigurationProfileId_,
        versionNumber = pVersionNumber_
      }

-- | The application ID.
deleteHostedConfigurationVersion_applicationId :: Lens.Lens' DeleteHostedConfigurationVersion Prelude.Text
deleteHostedConfigurationVersion_applicationId = Lens.lens (\DeleteHostedConfigurationVersion' {applicationId} -> applicationId) (\s@DeleteHostedConfigurationVersion' {} a -> s {applicationId = a} :: DeleteHostedConfigurationVersion)

-- | The configuration profile ID.
deleteHostedConfigurationVersion_configurationProfileId :: Lens.Lens' DeleteHostedConfigurationVersion Prelude.Text
deleteHostedConfigurationVersion_configurationProfileId = Lens.lens (\DeleteHostedConfigurationVersion' {configurationProfileId} -> configurationProfileId) (\s@DeleteHostedConfigurationVersion' {} a -> s {configurationProfileId = a} :: DeleteHostedConfigurationVersion)

-- | The versions number to delete.
deleteHostedConfigurationVersion_versionNumber :: Lens.Lens' DeleteHostedConfigurationVersion Prelude.Int
deleteHostedConfigurationVersion_versionNumber = Lens.lens (\DeleteHostedConfigurationVersion' {versionNumber} -> versionNumber) (\s@DeleteHostedConfigurationVersion' {} a -> s {versionNumber = a} :: DeleteHostedConfigurationVersion)

instance
  Core.AWSRequest
    DeleteHostedConfigurationVersion
  where
  type
    AWSResponse DeleteHostedConfigurationVersion =
      DeleteHostedConfigurationVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteHostedConfigurationVersionResponse'

instance
  Prelude.Hashable
    DeleteHostedConfigurationVersion
  where
  hashWithSalt
    _salt
    DeleteHostedConfigurationVersion' {..} =
      _salt `Prelude.hashWithSalt` applicationId
        `Prelude.hashWithSalt` configurationProfileId
        `Prelude.hashWithSalt` versionNumber

instance
  Prelude.NFData
    DeleteHostedConfigurationVersion
  where
  rnf DeleteHostedConfigurationVersion' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf configurationProfileId
      `Prelude.seq` Prelude.rnf versionNumber

instance
  Core.ToHeaders
    DeleteHostedConfigurationVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteHostedConfigurationVersion where
  toPath DeleteHostedConfigurationVersion' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/configurationprofiles/",
        Core.toBS configurationProfileId,
        "/hostedconfigurationversions/",
        Core.toBS versionNumber
      ]

instance
  Core.ToQuery
    DeleteHostedConfigurationVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHostedConfigurationVersionResponse' smart constructor.
data DeleteHostedConfigurationVersionResponse = DeleteHostedConfigurationVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHostedConfigurationVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteHostedConfigurationVersionResponse ::
  DeleteHostedConfigurationVersionResponse
newDeleteHostedConfigurationVersionResponse =
  DeleteHostedConfigurationVersionResponse'

instance
  Prelude.NFData
    DeleteHostedConfigurationVersionResponse
  where
  rnf _ = ()

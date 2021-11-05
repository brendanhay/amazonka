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
-- Module      : Network.AWS.AppConfig.DeleteHostedConfigurationVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a version of a configuration from the AppConfig configuration
-- store.
module Network.AWS.AppConfig.DeleteHostedConfigurationVersion
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

import Network.AWS.AppConfig.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteHostedConfigurationVersionResponse'

instance
  Prelude.Hashable
    DeleteHostedConfigurationVersion

instance
  Prelude.NFData
    DeleteHostedConfigurationVersion

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

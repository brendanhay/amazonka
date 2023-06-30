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
-- Module      : Amazonka.AppConfig.DeleteConfigurationProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration profile. Deleting a configuration profile does
-- not delete a configuration from a host.
module Amazonka.AppConfig.DeleteConfigurationProfile
  ( -- * Creating a Request
    DeleteConfigurationProfile (..),
    newDeleteConfigurationProfile,

    -- * Request Lenses
    deleteConfigurationProfile_applicationId,
    deleteConfigurationProfile_configurationProfileId,

    -- * Destructuring the Response
    DeleteConfigurationProfileResponse (..),
    newDeleteConfigurationProfileResponse,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConfigurationProfile' smart constructor.
data DeleteConfigurationProfile = DeleteConfigurationProfile'
  { -- | The application ID that includes the configuration profile you want to
    -- delete.
    applicationId :: Prelude.Text,
    -- | The ID of the configuration profile you want to delete.
    configurationProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteConfigurationProfile_applicationId' - The application ID that includes the configuration profile you want to
-- delete.
--
-- 'configurationProfileId', 'deleteConfigurationProfile_configurationProfileId' - The ID of the configuration profile you want to delete.
newDeleteConfigurationProfile ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'configurationProfileId'
  Prelude.Text ->
  DeleteConfigurationProfile
newDeleteConfigurationProfile
  pApplicationId_
  pConfigurationProfileId_ =
    DeleteConfigurationProfile'
      { applicationId =
          pApplicationId_,
        configurationProfileId =
          pConfigurationProfileId_
      }

-- | The application ID that includes the configuration profile you want to
-- delete.
deleteConfigurationProfile_applicationId :: Lens.Lens' DeleteConfigurationProfile Prelude.Text
deleteConfigurationProfile_applicationId = Lens.lens (\DeleteConfigurationProfile' {applicationId} -> applicationId) (\s@DeleteConfigurationProfile' {} a -> s {applicationId = a} :: DeleteConfigurationProfile)

-- | The ID of the configuration profile you want to delete.
deleteConfigurationProfile_configurationProfileId :: Lens.Lens' DeleteConfigurationProfile Prelude.Text
deleteConfigurationProfile_configurationProfileId = Lens.lens (\DeleteConfigurationProfile' {configurationProfileId} -> configurationProfileId) (\s@DeleteConfigurationProfile' {} a -> s {configurationProfileId = a} :: DeleteConfigurationProfile)

instance Core.AWSRequest DeleteConfigurationProfile where
  type
    AWSResponse DeleteConfigurationProfile =
      DeleteConfigurationProfileResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteConfigurationProfileResponse'

instance Prelude.Hashable DeleteConfigurationProfile where
  hashWithSalt _salt DeleteConfigurationProfile' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` configurationProfileId

instance Prelude.NFData DeleteConfigurationProfile where
  rnf DeleteConfigurationProfile' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf configurationProfileId

instance Data.ToHeaders DeleteConfigurationProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConfigurationProfile where
  toPath DeleteConfigurationProfile' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/configurationprofiles/",
        Data.toBS configurationProfileId
      ]

instance Data.ToQuery DeleteConfigurationProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigurationProfileResponse' smart constructor.
data DeleteConfigurationProfileResponse = DeleteConfigurationProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConfigurationProfileResponse ::
  DeleteConfigurationProfileResponse
newDeleteConfigurationProfileResponse =
  DeleteConfigurationProfileResponse'

instance
  Prelude.NFData
    DeleteConfigurationProfileResponse
  where
  rnf _ = ()

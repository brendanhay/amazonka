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
-- Module      : Amazonka.Detective.UpdateOrganizationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for the Organizations integration in the
-- current Region. Can only be called by the Detective administrator
-- account for the organization.
module Amazonka.Detective.UpdateOrganizationConfiguration
  ( -- * Creating a Request
    UpdateOrganizationConfiguration (..),
    newUpdateOrganizationConfiguration,

    -- * Request Lenses
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfiguration_graphArn,

    -- * Destructuring the Response
    UpdateOrganizationConfigurationResponse (..),
    newUpdateOrganizationConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateOrganizationConfiguration' smart constructor.
data UpdateOrganizationConfiguration = UpdateOrganizationConfiguration'
  { -- | Indicates whether to automatically enable new organization accounts as
    -- member accounts in the organization behavior graph.
    autoEnable :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the organization behavior graph.
    graphArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'updateOrganizationConfiguration_autoEnable' - Indicates whether to automatically enable new organization accounts as
-- member accounts in the organization behavior graph.
--
-- 'graphArn', 'updateOrganizationConfiguration_graphArn' - The ARN of the organization behavior graph.
newUpdateOrganizationConfiguration ::
  -- | 'graphArn'
  Prelude.Text ->
  UpdateOrganizationConfiguration
newUpdateOrganizationConfiguration pGraphArn_ =
  UpdateOrganizationConfiguration'
    { autoEnable =
        Prelude.Nothing,
      graphArn = pGraphArn_
    }

-- | Indicates whether to automatically enable new organization accounts as
-- member accounts in the organization behavior graph.
updateOrganizationConfiguration_autoEnable :: Lens.Lens' UpdateOrganizationConfiguration (Prelude.Maybe Prelude.Bool)
updateOrganizationConfiguration_autoEnable = Lens.lens (\UpdateOrganizationConfiguration' {autoEnable} -> autoEnable) (\s@UpdateOrganizationConfiguration' {} a -> s {autoEnable = a} :: UpdateOrganizationConfiguration)

-- | The ARN of the organization behavior graph.
updateOrganizationConfiguration_graphArn :: Lens.Lens' UpdateOrganizationConfiguration Prelude.Text
updateOrganizationConfiguration_graphArn = Lens.lens (\UpdateOrganizationConfiguration' {graphArn} -> graphArn) (\s@UpdateOrganizationConfiguration' {} a -> s {graphArn = a} :: UpdateOrganizationConfiguration)

instance
  Core.AWSRequest
    UpdateOrganizationConfiguration
  where
  type
    AWSResponse UpdateOrganizationConfiguration =
      UpdateOrganizationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateOrganizationConfigurationResponse'

instance
  Prelude.Hashable
    UpdateOrganizationConfiguration
  where
  hashWithSalt
    _salt
    UpdateOrganizationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` autoEnable
        `Prelude.hashWithSalt` graphArn

instance
  Prelude.NFData
    UpdateOrganizationConfiguration
  where
  rnf UpdateOrganizationConfiguration' {..} =
    Prelude.rnf autoEnable
      `Prelude.seq` Prelude.rnf graphArn

instance
  Data.ToHeaders
    UpdateOrganizationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateOrganizationConfiguration where
  toJSON UpdateOrganizationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoEnable" Data..=) Prelude.<$> autoEnable,
            Prelude.Just ("GraphArn" Data..= graphArn)
          ]
      )

instance Data.ToPath UpdateOrganizationConfiguration where
  toPath =
    Prelude.const
      "/orgs/updateOrganizationConfiguration"

instance Data.ToQuery UpdateOrganizationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOrganizationConfigurationResponse' smart constructor.
data UpdateOrganizationConfigurationResponse = UpdateOrganizationConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrganizationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateOrganizationConfigurationResponse ::
  UpdateOrganizationConfigurationResponse
newUpdateOrganizationConfigurationResponse =
  UpdateOrganizationConfigurationResponse'

instance
  Prelude.NFData
    UpdateOrganizationConfigurationResponse
  where
  rnf _ = ()

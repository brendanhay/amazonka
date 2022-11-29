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
-- Module      : Amazonka.Chime.DeleteAppInstanceStreamingConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the streaming configurations of an @AppInstance@.
module Amazonka.Chime.DeleteAppInstanceStreamingConfigurations
  ( -- * Creating a Request
    DeleteAppInstanceStreamingConfigurations (..),
    newDeleteAppInstanceStreamingConfigurations,

    -- * Request Lenses
    deleteAppInstanceStreamingConfigurations_appInstanceArn,

    -- * Destructuring the Response
    DeleteAppInstanceStreamingConfigurationsResponse (..),
    newDeleteAppInstanceStreamingConfigurationsResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAppInstanceStreamingConfigurations' smart constructor.
data DeleteAppInstanceStreamingConfigurations = DeleteAppInstanceStreamingConfigurations'
  { -- | The ARN of the streaming configurations being deleted.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInstanceStreamingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'deleteAppInstanceStreamingConfigurations_appInstanceArn' - The ARN of the streaming configurations being deleted.
newDeleteAppInstanceStreamingConfigurations ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  DeleteAppInstanceStreamingConfigurations
newDeleteAppInstanceStreamingConfigurations
  pAppInstanceArn_ =
    DeleteAppInstanceStreamingConfigurations'
      { appInstanceArn =
          pAppInstanceArn_
      }

-- | The ARN of the streaming configurations being deleted.
deleteAppInstanceStreamingConfigurations_appInstanceArn :: Lens.Lens' DeleteAppInstanceStreamingConfigurations Prelude.Text
deleteAppInstanceStreamingConfigurations_appInstanceArn = Lens.lens (\DeleteAppInstanceStreamingConfigurations' {appInstanceArn} -> appInstanceArn) (\s@DeleteAppInstanceStreamingConfigurations' {} a -> s {appInstanceArn = a} :: DeleteAppInstanceStreamingConfigurations)

instance
  Core.AWSRequest
    DeleteAppInstanceStreamingConfigurations
  where
  type
    AWSResponse
      DeleteAppInstanceStreamingConfigurations =
      DeleteAppInstanceStreamingConfigurationsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteAppInstanceStreamingConfigurationsResponse'

instance
  Prelude.Hashable
    DeleteAppInstanceStreamingConfigurations
  where
  hashWithSalt
    _salt
    DeleteAppInstanceStreamingConfigurations' {..} =
      _salt `Prelude.hashWithSalt` appInstanceArn

instance
  Prelude.NFData
    DeleteAppInstanceStreamingConfigurations
  where
  rnf DeleteAppInstanceStreamingConfigurations' {..} =
    Prelude.rnf appInstanceArn

instance
  Core.ToHeaders
    DeleteAppInstanceStreamingConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteAppInstanceStreamingConfigurations
  where
  toPath DeleteAppInstanceStreamingConfigurations' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Core.toBS appInstanceArn,
        "/streaming-configurations"
      ]

instance
  Core.ToQuery
    DeleteAppInstanceStreamingConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppInstanceStreamingConfigurationsResponse' smart constructor.
data DeleteAppInstanceStreamingConfigurationsResponse = DeleteAppInstanceStreamingConfigurationsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppInstanceStreamingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppInstanceStreamingConfigurationsResponse ::
  DeleteAppInstanceStreamingConfigurationsResponse
newDeleteAppInstanceStreamingConfigurationsResponse =
  DeleteAppInstanceStreamingConfigurationsResponse'

instance
  Prelude.NFData
    DeleteAppInstanceStreamingConfigurationsResponse
  where
  rnf _ = ()

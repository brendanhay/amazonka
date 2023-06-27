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
-- Module      : Amazonka.ChimeSDKMessaging.DeleteMessagingStreamingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the streaming configurations for an @AppInstance@. For more
-- information, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/streaming-export.html Streaming messaging data>
-- in the /Amazon Chime SDK Developer Guide/.
module Amazonka.ChimeSDKMessaging.DeleteMessagingStreamingConfigurations
  ( -- * Creating a Request
    DeleteMessagingStreamingConfigurations (..),
    newDeleteMessagingStreamingConfigurations,

    -- * Request Lenses
    deleteMessagingStreamingConfigurations_appInstanceArn,

    -- * Destructuring the Response
    DeleteMessagingStreamingConfigurationsResponse (..),
    newDeleteMessagingStreamingConfigurationsResponse,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMessagingStreamingConfigurations' smart constructor.
data DeleteMessagingStreamingConfigurations = DeleteMessagingStreamingConfigurations'
  { -- | The ARN of the streaming configurations being deleted.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessagingStreamingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'deleteMessagingStreamingConfigurations_appInstanceArn' - The ARN of the streaming configurations being deleted.
newDeleteMessagingStreamingConfigurations ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  DeleteMessagingStreamingConfigurations
newDeleteMessagingStreamingConfigurations
  pAppInstanceArn_ =
    DeleteMessagingStreamingConfigurations'
      { appInstanceArn =
          pAppInstanceArn_
      }

-- | The ARN of the streaming configurations being deleted.
deleteMessagingStreamingConfigurations_appInstanceArn :: Lens.Lens' DeleteMessagingStreamingConfigurations Prelude.Text
deleteMessagingStreamingConfigurations_appInstanceArn = Lens.lens (\DeleteMessagingStreamingConfigurations' {appInstanceArn} -> appInstanceArn) (\s@DeleteMessagingStreamingConfigurations' {} a -> s {appInstanceArn = a} :: DeleteMessagingStreamingConfigurations)

instance
  Core.AWSRequest
    DeleteMessagingStreamingConfigurations
  where
  type
    AWSResponse
      DeleteMessagingStreamingConfigurations =
      DeleteMessagingStreamingConfigurationsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteMessagingStreamingConfigurationsResponse'

instance
  Prelude.Hashable
    DeleteMessagingStreamingConfigurations
  where
  hashWithSalt
    _salt
    DeleteMessagingStreamingConfigurations' {..} =
      _salt `Prelude.hashWithSalt` appInstanceArn

instance
  Prelude.NFData
    DeleteMessagingStreamingConfigurations
  where
  rnf DeleteMessagingStreamingConfigurations' {..} =
    Prelude.rnf appInstanceArn

instance
  Data.ToHeaders
    DeleteMessagingStreamingConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteMessagingStreamingConfigurations
  where
  toPath DeleteMessagingStreamingConfigurations' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Data.toBS appInstanceArn,
        "/streaming-configurations"
      ]

instance
  Data.ToQuery
    DeleteMessagingStreamingConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMessagingStreamingConfigurationsResponse' smart constructor.
data DeleteMessagingStreamingConfigurationsResponse = DeleteMessagingStreamingConfigurationsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessagingStreamingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMessagingStreamingConfigurationsResponse ::
  DeleteMessagingStreamingConfigurationsResponse
newDeleteMessagingStreamingConfigurationsResponse =
  DeleteMessagingStreamingConfigurationsResponse'

instance
  Prelude.NFData
    DeleteMessagingStreamingConfigurationsResponse
  where
  rnf _ = ()

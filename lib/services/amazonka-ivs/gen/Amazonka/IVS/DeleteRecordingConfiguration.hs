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
-- Module      : Amazonka.IVS.DeleteRecordingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the recording configuration for the specified ARN.
--
-- If you try to delete a recording configuration that is associated with a
-- channel, you will get an error (409 ConflictException). To avoid this,
-- for all channels that reference the recording configuration, first use
-- UpdateChannel to set the @recordingConfigurationArn@ field to an empty
-- string, then use DeleteRecordingConfiguration.
module Amazonka.IVS.DeleteRecordingConfiguration
  ( -- * Creating a Request
    DeleteRecordingConfiguration (..),
    newDeleteRecordingConfiguration,

    -- * Request Lenses
    deleteRecordingConfiguration_arn,

    -- * Destructuring the Response
    DeleteRecordingConfigurationResponse (..),
    newDeleteRecordingConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRecordingConfiguration' smart constructor.
data DeleteRecordingConfiguration = DeleteRecordingConfiguration'
  { -- | ARN of the recording configuration to be deleted.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecordingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteRecordingConfiguration_arn' - ARN of the recording configuration to be deleted.
newDeleteRecordingConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  DeleteRecordingConfiguration
newDeleteRecordingConfiguration pArn_ =
  DeleteRecordingConfiguration' {arn = pArn_}

-- | ARN of the recording configuration to be deleted.
deleteRecordingConfiguration_arn :: Lens.Lens' DeleteRecordingConfiguration Prelude.Text
deleteRecordingConfiguration_arn = Lens.lens (\DeleteRecordingConfiguration' {arn} -> arn) (\s@DeleteRecordingConfiguration' {} a -> s {arn = a} :: DeleteRecordingConfiguration)

instance Core.AWSRequest DeleteRecordingConfiguration where
  type
    AWSResponse DeleteRecordingConfiguration =
      DeleteRecordingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteRecordingConfigurationResponse'

instance
  Prelude.Hashable
    DeleteRecordingConfiguration
  where
  hashWithSalt _salt DeleteRecordingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteRecordingConfiguration where
  rnf DeleteRecordingConfiguration' {..} =
    Prelude.rnf arn

instance Core.ToHeaders DeleteRecordingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteRecordingConfiguration where
  toJSON DeleteRecordingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Core..= arn)]
      )

instance Core.ToPath DeleteRecordingConfiguration where
  toPath =
    Prelude.const "/DeleteRecordingConfiguration"

instance Core.ToQuery DeleteRecordingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecordingConfigurationResponse' smart constructor.
data DeleteRecordingConfigurationResponse = DeleteRecordingConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecordingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRecordingConfigurationResponse ::
  DeleteRecordingConfigurationResponse
newDeleteRecordingConfigurationResponse =
  DeleteRecordingConfigurationResponse'

instance
  Prelude.NFData
    DeleteRecordingConfigurationResponse
  where
  rnf _ = ()

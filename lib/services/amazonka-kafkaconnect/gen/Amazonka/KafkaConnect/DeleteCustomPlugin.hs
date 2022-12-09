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
-- Module      : Amazonka.KafkaConnect.DeleteCustomPlugin
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom plugin.
module Amazonka.KafkaConnect.DeleteCustomPlugin
  ( -- * Creating a Request
    DeleteCustomPlugin (..),
    newDeleteCustomPlugin,

    -- * Request Lenses
    deleteCustomPlugin_customPluginArn,

    -- * Destructuring the Response
    DeleteCustomPluginResponse (..),
    newDeleteCustomPluginResponse,

    -- * Response Lenses
    deleteCustomPluginResponse_customPluginArn,
    deleteCustomPluginResponse_customPluginState,
    deleteCustomPluginResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomPlugin' smart constructor.
data DeleteCustomPlugin = DeleteCustomPlugin'
  { -- | The Amazon Resource Name (ARN) of the custom plugin that you want to
    -- delete.
    customPluginArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomPlugin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPluginArn', 'deleteCustomPlugin_customPluginArn' - The Amazon Resource Name (ARN) of the custom plugin that you want to
-- delete.
newDeleteCustomPlugin ::
  -- | 'customPluginArn'
  Prelude.Text ->
  DeleteCustomPlugin
newDeleteCustomPlugin pCustomPluginArn_ =
  DeleteCustomPlugin'
    { customPluginArn =
        pCustomPluginArn_
    }

-- | The Amazon Resource Name (ARN) of the custom plugin that you want to
-- delete.
deleteCustomPlugin_customPluginArn :: Lens.Lens' DeleteCustomPlugin Prelude.Text
deleteCustomPlugin_customPluginArn = Lens.lens (\DeleteCustomPlugin' {customPluginArn} -> customPluginArn) (\s@DeleteCustomPlugin' {} a -> s {customPluginArn = a} :: DeleteCustomPlugin)

instance Core.AWSRequest DeleteCustomPlugin where
  type
    AWSResponse DeleteCustomPlugin =
      DeleteCustomPluginResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCustomPluginResponse'
            Prelude.<$> (x Data..?> "customPluginArn")
            Prelude.<*> (x Data..?> "customPluginState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomPlugin where
  hashWithSalt _salt DeleteCustomPlugin' {..} =
    _salt `Prelude.hashWithSalt` customPluginArn

instance Prelude.NFData DeleteCustomPlugin where
  rnf DeleteCustomPlugin' {..} =
    Prelude.rnf customPluginArn

instance Data.ToHeaders DeleteCustomPlugin where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCustomPlugin where
  toPath DeleteCustomPlugin' {..} =
    Prelude.mconcat
      ["/v1/custom-plugins/", Data.toBS customPluginArn]

instance Data.ToQuery DeleteCustomPlugin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCustomPluginResponse' smart constructor.
data DeleteCustomPluginResponse = DeleteCustomPluginResponse'
  { -- | The Amazon Resource Name (ARN) of the custom plugin that you requested
    -- to delete.
    customPluginArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the custom plugin.
    customPluginState :: Prelude.Maybe CustomPluginState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomPluginResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPluginArn', 'deleteCustomPluginResponse_customPluginArn' - The Amazon Resource Name (ARN) of the custom plugin that you requested
-- to delete.
--
-- 'customPluginState', 'deleteCustomPluginResponse_customPluginState' - The state of the custom plugin.
--
-- 'httpStatus', 'deleteCustomPluginResponse_httpStatus' - The response's http status code.
newDeleteCustomPluginResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomPluginResponse
newDeleteCustomPluginResponse pHttpStatus_ =
  DeleteCustomPluginResponse'
    { customPluginArn =
        Prelude.Nothing,
      customPluginState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the custom plugin that you requested
-- to delete.
deleteCustomPluginResponse_customPluginArn :: Lens.Lens' DeleteCustomPluginResponse (Prelude.Maybe Prelude.Text)
deleteCustomPluginResponse_customPluginArn = Lens.lens (\DeleteCustomPluginResponse' {customPluginArn} -> customPluginArn) (\s@DeleteCustomPluginResponse' {} a -> s {customPluginArn = a} :: DeleteCustomPluginResponse)

-- | The state of the custom plugin.
deleteCustomPluginResponse_customPluginState :: Lens.Lens' DeleteCustomPluginResponse (Prelude.Maybe CustomPluginState)
deleteCustomPluginResponse_customPluginState = Lens.lens (\DeleteCustomPluginResponse' {customPluginState} -> customPluginState) (\s@DeleteCustomPluginResponse' {} a -> s {customPluginState = a} :: DeleteCustomPluginResponse)

-- | The response's http status code.
deleteCustomPluginResponse_httpStatus :: Lens.Lens' DeleteCustomPluginResponse Prelude.Int
deleteCustomPluginResponse_httpStatus = Lens.lens (\DeleteCustomPluginResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomPluginResponse' {} a -> s {httpStatus = a} :: DeleteCustomPluginResponse)

instance Prelude.NFData DeleteCustomPluginResponse where
  rnf DeleteCustomPluginResponse' {..} =
    Prelude.rnf customPluginArn
      `Prelude.seq` Prelude.rnf customPluginState
      `Prelude.seq` Prelude.rnf httpStatus

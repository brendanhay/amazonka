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
-- Module      : Amazonka.Kafka.DeleteConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an MSK Configuration.
module Amazonka.Kafka.DeleteConfiguration
  ( -- * Creating a Request
    DeleteConfiguration (..),
    newDeleteConfiguration,

    -- * Request Lenses
    deleteConfiguration_arn,

    -- * Destructuring the Response
    DeleteConfigurationResponse (..),
    newDeleteConfigurationResponse,

    -- * Response Lenses
    deleteConfigurationResponse_arn,
    deleteConfigurationResponse_state,
    deleteConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConfiguration' smart constructor.
data DeleteConfiguration = DeleteConfiguration'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
    -- configuration.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteConfiguration_arn' - The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration.
newDeleteConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  DeleteConfiguration
newDeleteConfiguration pArn_ =
  DeleteConfiguration' {arn = pArn_}

-- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration.
deleteConfiguration_arn :: Lens.Lens' DeleteConfiguration Prelude.Text
deleteConfiguration_arn = Lens.lens (\DeleteConfiguration' {arn} -> arn) (\s@DeleteConfiguration' {} a -> s {arn = a} :: DeleteConfiguration)

instance Core.AWSRequest DeleteConfiguration where
  type
    AWSResponse DeleteConfiguration =
      DeleteConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteConfigurationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConfiguration where
  hashWithSalt _salt DeleteConfiguration' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteConfiguration where
  rnf DeleteConfiguration' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConfiguration where
  toPath DeleteConfiguration' {..} =
    Prelude.mconcat
      ["/v1/configurations/", Data.toBS arn]

instance Data.ToQuery DeleteConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigurationResponse' smart constructor.
data DeleteConfigurationResponse = DeleteConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
    -- configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the configuration. The possible states are ACTIVE,
    -- DELETING, and DELETE_FAILED.
    state :: Prelude.Maybe ConfigurationState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteConfigurationResponse_arn' - The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration.
--
-- 'state', 'deleteConfigurationResponse_state' - The state of the configuration. The possible states are ACTIVE,
-- DELETING, and DELETE_FAILED.
--
-- 'httpStatus', 'deleteConfigurationResponse_httpStatus' - The response's http status code.
newDeleteConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConfigurationResponse
newDeleteConfigurationResponse pHttpStatus_ =
  DeleteConfigurationResponse'
    { arn = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) that uniquely identifies an MSK
-- configuration.
deleteConfigurationResponse_arn :: Lens.Lens' DeleteConfigurationResponse (Prelude.Maybe Prelude.Text)
deleteConfigurationResponse_arn = Lens.lens (\DeleteConfigurationResponse' {arn} -> arn) (\s@DeleteConfigurationResponse' {} a -> s {arn = a} :: DeleteConfigurationResponse)

-- | The state of the configuration. The possible states are ACTIVE,
-- DELETING, and DELETE_FAILED.
deleteConfigurationResponse_state :: Lens.Lens' DeleteConfigurationResponse (Prelude.Maybe ConfigurationState)
deleteConfigurationResponse_state = Lens.lens (\DeleteConfigurationResponse' {state} -> state) (\s@DeleteConfigurationResponse' {} a -> s {state = a} :: DeleteConfigurationResponse)

-- | The response's http status code.
deleteConfigurationResponse_httpStatus :: Lens.Lens' DeleteConfigurationResponse Prelude.Int
deleteConfigurationResponse_httpStatus = Lens.lens (\DeleteConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteConfigurationResponse)

instance Prelude.NFData DeleteConfigurationResponse where
  rnf DeleteConfigurationResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf state `Prelude.seq`
        Prelude.rnf httpStatus

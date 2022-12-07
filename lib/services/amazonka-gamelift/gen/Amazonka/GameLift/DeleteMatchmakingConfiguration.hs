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
-- Module      : Amazonka.GameLift.DeleteMatchmakingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently removes a FlexMatch matchmaking configuration. To delete,
-- specify the configuration name. A matchmaking configuration cannot be
-- deleted if it is being used in any active matchmaking tickets.
--
-- __Related actions__
--
-- CreateMatchmakingConfiguration | DescribeMatchmakingConfigurations |
-- UpdateMatchmakingConfiguration | DeleteMatchmakingConfiguration |
-- CreateMatchmakingRuleSet | DescribeMatchmakingRuleSets |
-- ValidateMatchmakingRuleSet | DeleteMatchmakingRuleSet |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DeleteMatchmakingConfiguration
  ( -- * Creating a Request
    DeleteMatchmakingConfiguration (..),
    newDeleteMatchmakingConfiguration,

    -- * Request Lenses
    deleteMatchmakingConfiguration_name,

    -- * Destructuring the Response
    DeleteMatchmakingConfigurationResponse (..),
    newDeleteMatchmakingConfigurationResponse,

    -- * Response Lenses
    deleteMatchmakingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteMatchmakingConfiguration' smart constructor.
data DeleteMatchmakingConfiguration = DeleteMatchmakingConfiguration'
  { -- | A unique identifier for the matchmaking configuration. You can use
    -- either the configuration name or ARN value.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMatchmakingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteMatchmakingConfiguration_name' - A unique identifier for the matchmaking configuration. You can use
-- either the configuration name or ARN value.
newDeleteMatchmakingConfiguration ::
  -- | 'name'
  Prelude.Text ->
  DeleteMatchmakingConfiguration
newDeleteMatchmakingConfiguration pName_ =
  DeleteMatchmakingConfiguration' {name = pName_}

-- | A unique identifier for the matchmaking configuration. You can use
-- either the configuration name or ARN value.
deleteMatchmakingConfiguration_name :: Lens.Lens' DeleteMatchmakingConfiguration Prelude.Text
deleteMatchmakingConfiguration_name = Lens.lens (\DeleteMatchmakingConfiguration' {name} -> name) (\s@DeleteMatchmakingConfiguration' {} a -> s {name = a} :: DeleteMatchmakingConfiguration)

instance
  Core.AWSRequest
    DeleteMatchmakingConfiguration
  where
  type
    AWSResponse DeleteMatchmakingConfiguration =
      DeleteMatchmakingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMatchmakingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteMatchmakingConfiguration
  where
  hashWithSalt
    _salt
    DeleteMatchmakingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    DeleteMatchmakingConfiguration
  where
  rnf DeleteMatchmakingConfiguration' {..} =
    Prelude.rnf name

instance
  Data.ToHeaders
    DeleteMatchmakingConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DeleteMatchmakingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMatchmakingConfiguration where
  toJSON DeleteMatchmakingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteMatchmakingConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMatchmakingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMatchmakingConfigurationResponse' smart constructor.
data DeleteMatchmakingConfigurationResponse = DeleteMatchmakingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMatchmakingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMatchmakingConfigurationResponse_httpStatus' - The response's http status code.
newDeleteMatchmakingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMatchmakingConfigurationResponse
newDeleteMatchmakingConfigurationResponse
  pHttpStatus_ =
    DeleteMatchmakingConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteMatchmakingConfigurationResponse_httpStatus :: Lens.Lens' DeleteMatchmakingConfigurationResponse Prelude.Int
deleteMatchmakingConfigurationResponse_httpStatus = Lens.lens (\DeleteMatchmakingConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteMatchmakingConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteMatchmakingConfigurationResponse)

instance
  Prelude.NFData
    DeleteMatchmakingConfigurationResponse
  where
  rnf DeleteMatchmakingConfigurationResponse' {..} =
    Prelude.rnf httpStatus

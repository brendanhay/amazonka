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
-- Module      : Amazonka.GameLift.UpdateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties for an alias. To update properties, specify the alias
-- ID to be updated and provide the information to be changed. To reassign
-- an alias to another fleet, provide an updated routing strategy. If
-- successful, the updated alias record is returned.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.UpdateAlias
  ( -- * Creating a Request
    UpdateAlias (..),
    newUpdateAlias,

    -- * Request Lenses
    updateAlias_description,
    updateAlias_name,
    updateAlias_routingStrategy,
    updateAlias_aliasId,

    -- * Destructuring the Response
    UpdateAliasResponse (..),
    newUpdateAliasResponse,

    -- * Response Lenses
    updateAliasResponse_alias,
    updateAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | A human-readable description of the alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with an alias. Alias names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The routing configuration, including routing type and fleet target, for
    -- the alias.
    routingStrategy :: Prelude.Maybe RoutingStrategy,
    -- | A unique identifier for the alias that you want to update. You can use
    -- either the alias ID or ARN value.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateAlias_description' - A human-readable description of the alias.
--
-- 'name', 'updateAlias_name' - A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
--
-- 'routingStrategy', 'updateAlias_routingStrategy' - The routing configuration, including routing type and fleet target, for
-- the alias.
--
-- 'aliasId', 'updateAlias_aliasId' - A unique identifier for the alias that you want to update. You can use
-- either the alias ID or ARN value.
newUpdateAlias ::
  -- | 'aliasId'
  Prelude.Text ->
  UpdateAlias
newUpdateAlias pAliasId_ =
  UpdateAlias'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      routingStrategy = Prelude.Nothing,
      aliasId = pAliasId_
    }

-- | A human-readable description of the alias.
updateAlias_description :: Lens.Lens' UpdateAlias (Prelude.Maybe Prelude.Text)
updateAlias_description = Lens.lens (\UpdateAlias' {description} -> description) (\s@UpdateAlias' {} a -> s {description = a} :: UpdateAlias)

-- | A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
updateAlias_name :: Lens.Lens' UpdateAlias (Prelude.Maybe Prelude.Text)
updateAlias_name = Lens.lens (\UpdateAlias' {name} -> name) (\s@UpdateAlias' {} a -> s {name = a} :: UpdateAlias)

-- | The routing configuration, including routing type and fleet target, for
-- the alias.
updateAlias_routingStrategy :: Lens.Lens' UpdateAlias (Prelude.Maybe RoutingStrategy)
updateAlias_routingStrategy = Lens.lens (\UpdateAlias' {routingStrategy} -> routingStrategy) (\s@UpdateAlias' {} a -> s {routingStrategy = a} :: UpdateAlias)

-- | A unique identifier for the alias that you want to update. You can use
-- either the alias ID or ARN value.
updateAlias_aliasId :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_aliasId = Lens.lens (\UpdateAlias' {aliasId} -> aliasId) (\s@UpdateAlias' {} a -> s {aliasId = a} :: UpdateAlias)

instance Core.AWSRequest UpdateAlias where
  type AWSResponse UpdateAlias = UpdateAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAliasResponse'
            Prelude.<$> (x Data..?> "Alias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAlias where
  hashWithSalt _salt UpdateAlias' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` routingStrategy
      `Prelude.hashWithSalt` aliasId

instance Prelude.NFData UpdateAlias where
  rnf UpdateAlias' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf routingStrategy
      `Prelude.seq` Prelude.rnf aliasId

instance Data.ToHeaders UpdateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.UpdateAlias" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("RoutingStrategy" Data..=)
              Prelude.<$> routingStrategy,
            Prelude.Just ("AliasId" Data..= aliasId)
          ]
      )

instance Data.ToPath UpdateAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  { -- | The updated alias resource.
    alias :: Prelude.Maybe Alias,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'updateAliasResponse_alias' - The updated alias resource.
--
-- 'httpStatus', 'updateAliasResponse_httpStatus' - The response's http status code.
newUpdateAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAliasResponse
newUpdateAliasResponse pHttpStatus_ =
  UpdateAliasResponse'
    { alias = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated alias resource.
updateAliasResponse_alias :: Lens.Lens' UpdateAliasResponse (Prelude.Maybe Alias)
updateAliasResponse_alias = Lens.lens (\UpdateAliasResponse' {alias} -> alias) (\s@UpdateAliasResponse' {} a -> s {alias = a} :: UpdateAliasResponse)

-- | The response's http status code.
updateAliasResponse_httpStatus :: Lens.Lens' UpdateAliasResponse Prelude.Int
updateAliasResponse_httpStatus = Lens.lens (\UpdateAliasResponse' {httpStatus} -> httpStatus) (\s@UpdateAliasResponse' {} a -> s {httpStatus = a} :: UpdateAliasResponse)

instance Prelude.NFData UpdateAliasResponse where
  rnf UpdateAliasResponse' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf httpStatus

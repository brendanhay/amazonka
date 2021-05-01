{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GameLift.UpdateAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   DescribeAlias
--
-- -   UpdateAlias
--
-- -   DeleteAlias
--
-- -   ResolveAlias
module Network.AWS.GameLift.UpdateAlias
  ( -- * Creating a Request
    UpdateAlias (..),
    newUpdateAlias,

    -- * Request Lenses
    updateAlias_routingStrategy,
    updateAlias_name,
    updateAlias_description,
    updateAlias_aliasId,

    -- * Destructuring the Response
    UpdateAliasResponse (..),
    newUpdateAliasResponse,

    -- * Response Lenses
    updateAliasResponse_alias,
    updateAliasResponse_httpStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | The routing configuration, including routing type and fleet target, for
    -- the alias.
    routingStrategy :: Prelude.Maybe RoutingStrategy,
    -- | A descriptive label that is associated with an alias. Alias names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | A human-readable description of the alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the alias that you want to update. You can use
    -- either the alias ID or ARN value.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingStrategy', 'updateAlias_routingStrategy' - The routing configuration, including routing type and fleet target, for
-- the alias.
--
-- 'name', 'updateAlias_name' - A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
--
-- 'description', 'updateAlias_description' - A human-readable description of the alias.
--
-- 'aliasId', 'updateAlias_aliasId' - A unique identifier for the alias that you want to update. You can use
-- either the alias ID or ARN value.
newUpdateAlias ::
  -- | 'aliasId'
  Prelude.Text ->
  UpdateAlias
newUpdateAlias pAliasId_ =
  UpdateAlias'
    { routingStrategy = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      aliasId = pAliasId_
    }

-- | The routing configuration, including routing type and fleet target, for
-- the alias.
updateAlias_routingStrategy :: Lens.Lens' UpdateAlias (Prelude.Maybe RoutingStrategy)
updateAlias_routingStrategy = Lens.lens (\UpdateAlias' {routingStrategy} -> routingStrategy) (\s@UpdateAlias' {} a -> s {routingStrategy = a} :: UpdateAlias)

-- | A descriptive label that is associated with an alias. Alias names do not
-- need to be unique.
updateAlias_name :: Lens.Lens' UpdateAlias (Prelude.Maybe Prelude.Text)
updateAlias_name = Lens.lens (\UpdateAlias' {name} -> name) (\s@UpdateAlias' {} a -> s {name = a} :: UpdateAlias)

-- | A human-readable description of the alias.
updateAlias_description :: Lens.Lens' UpdateAlias (Prelude.Maybe Prelude.Text)
updateAlias_description = Lens.lens (\UpdateAlias' {description} -> description) (\s@UpdateAlias' {} a -> s {description = a} :: UpdateAlias)

-- | A unique identifier for the alias that you want to update. You can use
-- either the alias ID or ARN value.
updateAlias_aliasId :: Lens.Lens' UpdateAlias Prelude.Text
updateAlias_aliasId = Lens.lens (\UpdateAlias' {aliasId} -> aliasId) (\s@UpdateAlias' {} a -> s {aliasId = a} :: UpdateAlias)

instance Prelude.AWSRequest UpdateAlias where
  type Rs UpdateAlias = UpdateAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAliasResponse'
            Prelude.<$> (x Prelude..?> "Alias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAlias

instance Prelude.NFData UpdateAlias

instance Prelude.ToHeaders UpdateAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.UpdateAlias" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoutingStrategy" Prelude..=)
              Prelude.<$> routingStrategy,
            ("Name" Prelude..=) Prelude.<$> name,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("AliasId" Prelude..= aliasId)
          ]
      )

instance Prelude.ToPath UpdateAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateAlias where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateAliasResponse' smart constructor.
data UpdateAliasResponse = UpdateAliasResponse'
  { -- | The updated alias resource.
    alias :: Prelude.Maybe Alias,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateAliasResponse

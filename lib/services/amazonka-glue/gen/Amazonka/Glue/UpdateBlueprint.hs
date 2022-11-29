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
-- Module      : Amazonka.Glue.UpdateBlueprint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered blueprint.
module Amazonka.Glue.UpdateBlueprint
  ( -- * Creating a Request
    UpdateBlueprint (..),
    newUpdateBlueprint,

    -- * Request Lenses
    updateBlueprint_description,
    updateBlueprint_name,
    updateBlueprint_blueprintLocation,

    -- * Destructuring the Response
    UpdateBlueprintResponse (..),
    newUpdateBlueprintResponse,

    -- * Response Lenses
    updateBlueprintResponse_name,
    updateBlueprintResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBlueprint' smart constructor.
data UpdateBlueprint = UpdateBlueprint'
  { -- | A description of the blueprint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the blueprint.
    name :: Prelude.Text,
    -- | Specifies a path in Amazon S3 where the blueprint is published.
    blueprintLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBlueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateBlueprint_description' - A description of the blueprint.
--
-- 'name', 'updateBlueprint_name' - The name of the blueprint.
--
-- 'blueprintLocation', 'updateBlueprint_blueprintLocation' - Specifies a path in Amazon S3 where the blueprint is published.
newUpdateBlueprint ::
  -- | 'name'
  Prelude.Text ->
  -- | 'blueprintLocation'
  Prelude.Text ->
  UpdateBlueprint
newUpdateBlueprint pName_ pBlueprintLocation_ =
  UpdateBlueprint'
    { description = Prelude.Nothing,
      name = pName_,
      blueprintLocation = pBlueprintLocation_
    }

-- | A description of the blueprint.
updateBlueprint_description :: Lens.Lens' UpdateBlueprint (Prelude.Maybe Prelude.Text)
updateBlueprint_description = Lens.lens (\UpdateBlueprint' {description} -> description) (\s@UpdateBlueprint' {} a -> s {description = a} :: UpdateBlueprint)

-- | The name of the blueprint.
updateBlueprint_name :: Lens.Lens' UpdateBlueprint Prelude.Text
updateBlueprint_name = Lens.lens (\UpdateBlueprint' {name} -> name) (\s@UpdateBlueprint' {} a -> s {name = a} :: UpdateBlueprint)

-- | Specifies a path in Amazon S3 where the blueprint is published.
updateBlueprint_blueprintLocation :: Lens.Lens' UpdateBlueprint Prelude.Text
updateBlueprint_blueprintLocation = Lens.lens (\UpdateBlueprint' {blueprintLocation} -> blueprintLocation) (\s@UpdateBlueprint' {} a -> s {blueprintLocation = a} :: UpdateBlueprint)

instance Core.AWSRequest UpdateBlueprint where
  type
    AWSResponse UpdateBlueprint =
      UpdateBlueprintResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBlueprintResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBlueprint where
  hashWithSalt _salt UpdateBlueprint' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` blueprintLocation

instance Prelude.NFData UpdateBlueprint where
  rnf UpdateBlueprint' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf blueprintLocation

instance Core.ToHeaders UpdateBlueprint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateBlueprint" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBlueprint where
  toJSON UpdateBlueprint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("BlueprintLocation" Core..= blueprintLocation)
          ]
      )

instance Core.ToPath UpdateBlueprint where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateBlueprint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBlueprintResponse' smart constructor.
data UpdateBlueprintResponse = UpdateBlueprintResponse'
  { -- | Returns the name of the blueprint that was updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBlueprintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateBlueprintResponse_name' - Returns the name of the blueprint that was updated.
--
-- 'httpStatus', 'updateBlueprintResponse_httpStatus' - The response's http status code.
newUpdateBlueprintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBlueprintResponse
newUpdateBlueprintResponse pHttpStatus_ =
  UpdateBlueprintResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the name of the blueprint that was updated.
updateBlueprintResponse_name :: Lens.Lens' UpdateBlueprintResponse (Prelude.Maybe Prelude.Text)
updateBlueprintResponse_name = Lens.lens (\UpdateBlueprintResponse' {name} -> name) (\s@UpdateBlueprintResponse' {} a -> s {name = a} :: UpdateBlueprintResponse)

-- | The response's http status code.
updateBlueprintResponse_httpStatus :: Lens.Lens' UpdateBlueprintResponse Prelude.Int
updateBlueprintResponse_httpStatus = Lens.lens (\UpdateBlueprintResponse' {httpStatus} -> httpStatus) (\s@UpdateBlueprintResponse' {} a -> s {httpStatus = a} :: UpdateBlueprintResponse)

instance Prelude.NFData UpdateBlueprintResponse where
  rnf UpdateBlueprintResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus

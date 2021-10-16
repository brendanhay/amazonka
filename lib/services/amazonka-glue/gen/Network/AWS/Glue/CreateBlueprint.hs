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
-- Module      : Network.AWS.Glue.CreateBlueprint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a blueprint with Glue.
module Network.AWS.Glue.CreateBlueprint
  ( -- * Creating a Request
    CreateBlueprint (..),
    newCreateBlueprint,

    -- * Request Lenses
    createBlueprint_tags,
    createBlueprint_description,
    createBlueprint_name,
    createBlueprint_blueprintLocation,

    -- * Destructuring the Response
    CreateBlueprintResponse (..),
    newCreateBlueprintResponse,

    -- * Response Lenses
    createBlueprintResponse_name,
    createBlueprintResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBlueprint' smart constructor.
data CreateBlueprint = CreateBlueprint'
  { -- | The tags to be applied to this blueprint.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the blueprint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the blueprint.
    name :: Prelude.Text,
    -- | Specifies a path in Amazon S3 where the blueprint is published.
    blueprintLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBlueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createBlueprint_tags' - The tags to be applied to this blueprint.
--
-- 'description', 'createBlueprint_description' - A description of the blueprint.
--
-- 'name', 'createBlueprint_name' - The name of the blueprint.
--
-- 'blueprintLocation', 'createBlueprint_blueprintLocation' - Specifies a path in Amazon S3 where the blueprint is published.
newCreateBlueprint ::
  -- | 'name'
  Prelude.Text ->
  -- | 'blueprintLocation'
  Prelude.Text ->
  CreateBlueprint
newCreateBlueprint pName_ pBlueprintLocation_ =
  CreateBlueprint'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      blueprintLocation = pBlueprintLocation_
    }

-- | The tags to be applied to this blueprint.
createBlueprint_tags :: Lens.Lens' CreateBlueprint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBlueprint_tags = Lens.lens (\CreateBlueprint' {tags} -> tags) (\s@CreateBlueprint' {} a -> s {tags = a} :: CreateBlueprint) Prelude.. Lens.mapping Lens._Coerce

-- | A description of the blueprint.
createBlueprint_description :: Lens.Lens' CreateBlueprint (Prelude.Maybe Prelude.Text)
createBlueprint_description = Lens.lens (\CreateBlueprint' {description} -> description) (\s@CreateBlueprint' {} a -> s {description = a} :: CreateBlueprint)

-- | The name of the blueprint.
createBlueprint_name :: Lens.Lens' CreateBlueprint Prelude.Text
createBlueprint_name = Lens.lens (\CreateBlueprint' {name} -> name) (\s@CreateBlueprint' {} a -> s {name = a} :: CreateBlueprint)

-- | Specifies a path in Amazon S3 where the blueprint is published.
createBlueprint_blueprintLocation :: Lens.Lens' CreateBlueprint Prelude.Text
createBlueprint_blueprintLocation = Lens.lens (\CreateBlueprint' {blueprintLocation} -> blueprintLocation) (\s@CreateBlueprint' {} a -> s {blueprintLocation = a} :: CreateBlueprint)

instance Core.AWSRequest CreateBlueprint where
  type
    AWSResponse CreateBlueprint =
      CreateBlueprintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBlueprintResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBlueprint

instance Prelude.NFData CreateBlueprint

instance Core.ToHeaders CreateBlueprint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateBlueprint" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBlueprint where
  toJSON CreateBlueprint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("BlueprintLocation" Core..= blueprintLocation)
          ]
      )

instance Core.ToPath CreateBlueprint where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBlueprint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBlueprintResponse' smart constructor.
data CreateBlueprintResponse = CreateBlueprintResponse'
  { -- | Returns the name of the blueprint that was registered.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBlueprintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createBlueprintResponse_name' - Returns the name of the blueprint that was registered.
--
-- 'httpStatus', 'createBlueprintResponse_httpStatus' - The response's http status code.
newCreateBlueprintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBlueprintResponse
newCreateBlueprintResponse pHttpStatus_ =
  CreateBlueprintResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the name of the blueprint that was registered.
createBlueprintResponse_name :: Lens.Lens' CreateBlueprintResponse (Prelude.Maybe Prelude.Text)
createBlueprintResponse_name = Lens.lens (\CreateBlueprintResponse' {name} -> name) (\s@CreateBlueprintResponse' {} a -> s {name = a} :: CreateBlueprintResponse)

-- | The response's http status code.
createBlueprintResponse_httpStatus :: Lens.Lens' CreateBlueprintResponse Prelude.Int
createBlueprintResponse_httpStatus = Lens.lens (\CreateBlueprintResponse' {httpStatus} -> httpStatus) (\s@CreateBlueprintResponse' {} a -> s {httpStatus = a} :: CreateBlueprintResponse)

instance Prelude.NFData CreateBlueprintResponse

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
-- Module      : Network.AWS.Greengrass.UpdateResourceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a resource definition.
module Network.AWS.Greengrass.UpdateResourceDefinition
  ( -- * Creating a Request
    UpdateResourceDefinition (..),
    newUpdateResourceDefinition,

    -- * Request Lenses
    updateResourceDefinition_name,
    updateResourceDefinition_resourceDefinitionId,

    -- * Destructuring the Response
    UpdateResourceDefinitionResponse (..),
    newUpdateResourceDefinitionResponse,

    -- * Response Lenses
    updateResourceDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateResourceDefinition' smart constructor.
data UpdateResourceDefinition = UpdateResourceDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource definition.
    resourceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateResourceDefinition_name' - The name of the definition.
--
-- 'resourceDefinitionId', 'updateResourceDefinition_resourceDefinitionId' - The ID of the resource definition.
newUpdateResourceDefinition ::
  -- | 'resourceDefinitionId'
  Prelude.Text ->
  UpdateResourceDefinition
newUpdateResourceDefinition pResourceDefinitionId_ =
  UpdateResourceDefinition'
    { name = Prelude.Nothing,
      resourceDefinitionId = pResourceDefinitionId_
    }

-- | The name of the definition.
updateResourceDefinition_name :: Lens.Lens' UpdateResourceDefinition (Prelude.Maybe Prelude.Text)
updateResourceDefinition_name = Lens.lens (\UpdateResourceDefinition' {name} -> name) (\s@UpdateResourceDefinition' {} a -> s {name = a} :: UpdateResourceDefinition)

-- | The ID of the resource definition.
updateResourceDefinition_resourceDefinitionId :: Lens.Lens' UpdateResourceDefinition Prelude.Text
updateResourceDefinition_resourceDefinitionId = Lens.lens (\UpdateResourceDefinition' {resourceDefinitionId} -> resourceDefinitionId) (\s@UpdateResourceDefinition' {} a -> s {resourceDefinitionId = a} :: UpdateResourceDefinition)

instance Prelude.AWSRequest UpdateResourceDefinition where
  type
    Rs UpdateResourceDefinition =
      UpdateResourceDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourceDefinition

instance Prelude.NFData UpdateResourceDefinition

instance Prelude.ToHeaders UpdateResourceDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateResourceDefinition where
  toJSON UpdateResourceDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Name" Prelude..=) Prelude.<$> name]
      )

instance Prelude.ToPath UpdateResourceDefinition where
  toPath UpdateResourceDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/resources/",
        Prelude.toBS resourceDefinitionId
      ]

instance Prelude.ToQuery UpdateResourceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceDefinitionResponse' smart constructor.
data UpdateResourceDefinitionResponse = UpdateResourceDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourceDefinitionResponse_httpStatus' - The response's http status code.
newUpdateResourceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceDefinitionResponse
newUpdateResourceDefinitionResponse pHttpStatus_ =
  UpdateResourceDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateResourceDefinitionResponse_httpStatus :: Lens.Lens' UpdateResourceDefinitionResponse Prelude.Int
updateResourceDefinitionResponse_httpStatus = Lens.lens (\UpdateResourceDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateResourceDefinitionResponse)

instance
  Prelude.NFData
    UpdateResourceDefinitionResponse

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
-- Module      : Network.AWS.Greengrass.UpdateCoreDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a core definition.
module Network.AWS.Greengrass.UpdateCoreDefinition
  ( -- * Creating a Request
    UpdateCoreDefinition (..),
    newUpdateCoreDefinition,

    -- * Request Lenses
    updateCoreDefinition_name,
    updateCoreDefinition_coreDefinitionId,

    -- * Destructuring the Response
    UpdateCoreDefinitionResponse (..),
    newUpdateCoreDefinitionResponse,

    -- * Response Lenses
    updateCoreDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCoreDefinition' smart constructor.
data UpdateCoreDefinition = UpdateCoreDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the core definition.
    coreDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCoreDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateCoreDefinition_name' - The name of the definition.
--
-- 'coreDefinitionId', 'updateCoreDefinition_coreDefinitionId' - The ID of the core definition.
newUpdateCoreDefinition ::
  -- | 'coreDefinitionId'
  Prelude.Text ->
  UpdateCoreDefinition
newUpdateCoreDefinition pCoreDefinitionId_ =
  UpdateCoreDefinition'
    { name = Prelude.Nothing,
      coreDefinitionId = pCoreDefinitionId_
    }

-- | The name of the definition.
updateCoreDefinition_name :: Lens.Lens' UpdateCoreDefinition (Prelude.Maybe Prelude.Text)
updateCoreDefinition_name = Lens.lens (\UpdateCoreDefinition' {name} -> name) (\s@UpdateCoreDefinition' {} a -> s {name = a} :: UpdateCoreDefinition)

-- | The ID of the core definition.
updateCoreDefinition_coreDefinitionId :: Lens.Lens' UpdateCoreDefinition Prelude.Text
updateCoreDefinition_coreDefinitionId = Lens.lens (\UpdateCoreDefinition' {coreDefinitionId} -> coreDefinitionId) (\s@UpdateCoreDefinition' {} a -> s {coreDefinitionId = a} :: UpdateCoreDefinition)

instance Prelude.AWSRequest UpdateCoreDefinition where
  type
    Rs UpdateCoreDefinition =
      UpdateCoreDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCoreDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCoreDefinition

instance Prelude.NFData UpdateCoreDefinition

instance Prelude.ToHeaders UpdateCoreDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateCoreDefinition where
  toJSON UpdateCoreDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Name" Prelude..=) Prelude.<$> name]
      )

instance Prelude.ToPath UpdateCoreDefinition where
  toPath UpdateCoreDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Prelude.toBS coreDefinitionId
      ]

instance Prelude.ToQuery UpdateCoreDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCoreDefinitionResponse' smart constructor.
data UpdateCoreDefinitionResponse = UpdateCoreDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCoreDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCoreDefinitionResponse_httpStatus' - The response's http status code.
newUpdateCoreDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCoreDefinitionResponse
newUpdateCoreDefinitionResponse pHttpStatus_ =
  UpdateCoreDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateCoreDefinitionResponse_httpStatus :: Lens.Lens' UpdateCoreDefinitionResponse Prelude.Int
updateCoreDefinitionResponse_httpStatus = Lens.lens (\UpdateCoreDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateCoreDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateCoreDefinitionResponse)

instance Prelude.NFData UpdateCoreDefinitionResponse

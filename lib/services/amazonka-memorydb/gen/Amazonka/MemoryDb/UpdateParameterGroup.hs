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
-- Module      : Amazonka.MemoryDb.UpdateParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameters of a parameter group. You can modify up to 20
-- parameters in a single request by submitting a list parameter name and
-- value pairs.
module Amazonka.MemoryDb.UpdateParameterGroup
  ( -- * Creating a Request
    UpdateParameterGroup (..),
    newUpdateParameterGroup,

    -- * Request Lenses
    updateParameterGroup_parameterGroupName,
    updateParameterGroup_parameterNameValues,

    -- * Destructuring the Response
    UpdateParameterGroupResponse (..),
    newUpdateParameterGroupResponse,

    -- * Response Lenses
    updateParameterGroupResponse_parameterGroup,
    updateParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateParameterGroup' smart constructor.
data UpdateParameterGroup = UpdateParameterGroup'
  { -- | The name of the parameter group to update.
    parameterGroupName :: Prelude.Text,
    -- | An array of parameter names and values for the parameter update. You
    -- must supply at least one parameter name and value; subsequent arguments
    -- are optional. A maximum of 20 parameters may be updated per request.
    parameterNameValues :: [ParameterNameValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupName', 'updateParameterGroup_parameterGroupName' - The name of the parameter group to update.
--
-- 'parameterNameValues', 'updateParameterGroup_parameterNameValues' - An array of parameter names and values for the parameter update. You
-- must supply at least one parameter name and value; subsequent arguments
-- are optional. A maximum of 20 parameters may be updated per request.
newUpdateParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  UpdateParameterGroup
newUpdateParameterGroup pParameterGroupName_ =
  UpdateParameterGroup'
    { parameterGroupName =
        pParameterGroupName_,
      parameterNameValues = Prelude.mempty
    }

-- | The name of the parameter group to update.
updateParameterGroup_parameterGroupName :: Lens.Lens' UpdateParameterGroup Prelude.Text
updateParameterGroup_parameterGroupName = Lens.lens (\UpdateParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@UpdateParameterGroup' {} a -> s {parameterGroupName = a} :: UpdateParameterGroup)

-- | An array of parameter names and values for the parameter update. You
-- must supply at least one parameter name and value; subsequent arguments
-- are optional. A maximum of 20 parameters may be updated per request.
updateParameterGroup_parameterNameValues :: Lens.Lens' UpdateParameterGroup [ParameterNameValue]
updateParameterGroup_parameterNameValues = Lens.lens (\UpdateParameterGroup' {parameterNameValues} -> parameterNameValues) (\s@UpdateParameterGroup' {} a -> s {parameterNameValues = a} :: UpdateParameterGroup) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateParameterGroup where
  type
    AWSResponse UpdateParameterGroup =
      UpdateParameterGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateParameterGroupResponse'
            Prelude.<$> (x Core..?> "ParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateParameterGroup where
  hashWithSalt _salt UpdateParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` parameterNameValues

instance Prelude.NFData UpdateParameterGroup where
  rnf UpdateParameterGroup' {..} =
    Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf parameterNameValues

instance Core.ToHeaders UpdateParameterGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.UpdateParameterGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateParameterGroup where
  toJSON UpdateParameterGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ParameterGroupName" Core..= parameterGroupName),
            Prelude.Just
              ("ParameterNameValues" Core..= parameterNameValues)
          ]
      )

instance Core.ToPath UpdateParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateParameterGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateParameterGroupResponse' smart constructor.
data UpdateParameterGroupResponse = UpdateParameterGroupResponse'
  { -- | The updated parameter group
    parameterGroup :: Prelude.Maybe ParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroup', 'updateParameterGroupResponse_parameterGroup' - The updated parameter group
--
-- 'httpStatus', 'updateParameterGroupResponse_httpStatus' - The response's http status code.
newUpdateParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateParameterGroupResponse
newUpdateParameterGroupResponse pHttpStatus_ =
  UpdateParameterGroupResponse'
    { parameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated parameter group
updateParameterGroupResponse_parameterGroup :: Lens.Lens' UpdateParameterGroupResponse (Prelude.Maybe ParameterGroup)
updateParameterGroupResponse_parameterGroup = Lens.lens (\UpdateParameterGroupResponse' {parameterGroup} -> parameterGroup) (\s@UpdateParameterGroupResponse' {} a -> s {parameterGroup = a} :: UpdateParameterGroupResponse)

-- | The response's http status code.
updateParameterGroupResponse_httpStatus :: Lens.Lens' UpdateParameterGroupResponse Prelude.Int
updateParameterGroupResponse_httpStatus = Lens.lens (\UpdateParameterGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateParameterGroupResponse' {} a -> s {httpStatus = a} :: UpdateParameterGroupResponse)

instance Prelude.NFData UpdateParameterGroupResponse where
  rnf UpdateParameterGroupResponse' {..} =
    Prelude.rnf parameterGroup
      `Prelude.seq` Prelude.rnf httpStatus

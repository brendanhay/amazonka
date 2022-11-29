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
-- Module      : Amazonka.MemoryDb.ResetParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a parameter group to the engine or system
-- default value. You can reset specific parameters by submitting a list of
-- parameter names. To reset the entire parameter group, specify the
-- AllParameters and ParameterGroupName parameters.
module Amazonka.MemoryDb.ResetParameterGroup
  ( -- * Creating a Request
    ResetParameterGroup (..),
    newResetParameterGroup,

    -- * Request Lenses
    resetParameterGroup_parameterNames,
    resetParameterGroup_allParameters,
    resetParameterGroup_parameterGroupName,

    -- * Destructuring the Response
    ResetParameterGroupResponse (..),
    newResetParameterGroupResponse,

    -- * Response Lenses
    resetParameterGroupResponse_parameterGroup,
    resetParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetParameterGroup' smart constructor.
data ResetParameterGroup = ResetParameterGroup'
  { -- | An array of parameter names to reset to their default values. If
    -- AllParameters is true, do not use ParameterNames. If AllParameters is
    -- false, you must specify the name of at least one parameter to reset.
    parameterNames :: Prelude.Maybe [Prelude.Text],
    -- | If true, all parameters in the parameter group are reset to their
    -- default values. If false, only the parameters listed by ParameterNames
    -- are reset to their default values.
    allParameters :: Prelude.Maybe Prelude.Bool,
    -- | The name of the parameter group to reset.
    parameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterNames', 'resetParameterGroup_parameterNames' - An array of parameter names to reset to their default values. If
-- AllParameters is true, do not use ParameterNames. If AllParameters is
-- false, you must specify the name of at least one parameter to reset.
--
-- 'allParameters', 'resetParameterGroup_allParameters' - If true, all parameters in the parameter group are reset to their
-- default values. If false, only the parameters listed by ParameterNames
-- are reset to their default values.
--
-- 'parameterGroupName', 'resetParameterGroup_parameterGroupName' - The name of the parameter group to reset.
newResetParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  ResetParameterGroup
newResetParameterGroup pParameterGroupName_ =
  ResetParameterGroup'
    { parameterNames =
        Prelude.Nothing,
      allParameters = Prelude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | An array of parameter names to reset to their default values. If
-- AllParameters is true, do not use ParameterNames. If AllParameters is
-- false, you must specify the name of at least one parameter to reset.
resetParameterGroup_parameterNames :: Lens.Lens' ResetParameterGroup (Prelude.Maybe [Prelude.Text])
resetParameterGroup_parameterNames = Lens.lens (\ResetParameterGroup' {parameterNames} -> parameterNames) (\s@ResetParameterGroup' {} a -> s {parameterNames = a} :: ResetParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | If true, all parameters in the parameter group are reset to their
-- default values. If false, only the parameters listed by ParameterNames
-- are reset to their default values.
resetParameterGroup_allParameters :: Lens.Lens' ResetParameterGroup (Prelude.Maybe Prelude.Bool)
resetParameterGroup_allParameters = Lens.lens (\ResetParameterGroup' {allParameters} -> allParameters) (\s@ResetParameterGroup' {} a -> s {allParameters = a} :: ResetParameterGroup)

-- | The name of the parameter group to reset.
resetParameterGroup_parameterGroupName :: Lens.Lens' ResetParameterGroup Prelude.Text
resetParameterGroup_parameterGroupName = Lens.lens (\ResetParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@ResetParameterGroup' {} a -> s {parameterGroupName = a} :: ResetParameterGroup)

instance Core.AWSRequest ResetParameterGroup where
  type
    AWSResponse ResetParameterGroup =
      ResetParameterGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetParameterGroupResponse'
            Prelude.<$> (x Core..?> "ParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetParameterGroup where
  hashWithSalt _salt ResetParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` parameterNames
      `Prelude.hashWithSalt` allParameters
      `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData ResetParameterGroup where
  rnf ResetParameterGroup' {..} =
    Prelude.rnf parameterNames
      `Prelude.seq` Prelude.rnf allParameters
      `Prelude.seq` Prelude.rnf parameterGroupName

instance Core.ToHeaders ResetParameterGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.ResetParameterGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ResetParameterGroup where
  toJSON ResetParameterGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParameterNames" Core..=)
              Prelude.<$> parameterNames,
            ("AllParameters" Core..=) Prelude.<$> allParameters,
            Prelude.Just
              ("ParameterGroupName" Core..= parameterGroupName)
          ]
      )

instance Core.ToPath ResetParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ResetParameterGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetParameterGroupResponse' smart constructor.
data ResetParameterGroupResponse = ResetParameterGroupResponse'
  { -- | The parameter group being reset.
    parameterGroup :: Prelude.Maybe ParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroup', 'resetParameterGroupResponse_parameterGroup' - The parameter group being reset.
--
-- 'httpStatus', 'resetParameterGroupResponse_httpStatus' - The response's http status code.
newResetParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetParameterGroupResponse
newResetParameterGroupResponse pHttpStatus_ =
  ResetParameterGroupResponse'
    { parameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The parameter group being reset.
resetParameterGroupResponse_parameterGroup :: Lens.Lens' ResetParameterGroupResponse (Prelude.Maybe ParameterGroup)
resetParameterGroupResponse_parameterGroup = Lens.lens (\ResetParameterGroupResponse' {parameterGroup} -> parameterGroup) (\s@ResetParameterGroupResponse' {} a -> s {parameterGroup = a} :: ResetParameterGroupResponse)

-- | The response's http status code.
resetParameterGroupResponse_httpStatus :: Lens.Lens' ResetParameterGroupResponse Prelude.Int
resetParameterGroupResponse_httpStatus = Lens.lens (\ResetParameterGroupResponse' {httpStatus} -> httpStatus) (\s@ResetParameterGroupResponse' {} a -> s {httpStatus = a} :: ResetParameterGroupResponse)

instance Prelude.NFData ResetParameterGroupResponse where
  rnf ResetParameterGroupResponse' {..} =
    Prelude.rnf parameterGroup
      `Prelude.seq` Prelude.rnf httpStatus

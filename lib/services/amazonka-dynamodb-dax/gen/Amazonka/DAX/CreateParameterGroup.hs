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
-- Module      : Amazonka.DAX.CreateParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new parameter group. A parameter group is a collection of
-- parameters that you apply to all of the nodes in a DAX cluster.
module Amazonka.DAX.CreateParameterGroup
  ( -- * Creating a Request
    CreateParameterGroup (..),
    newCreateParameterGroup,

    -- * Request Lenses
    createParameterGroup_description,
    createParameterGroup_parameterGroupName,

    -- * Destructuring the Response
    CreateParameterGroupResponse (..),
    newCreateParameterGroupResponse,

    -- * Response Lenses
    createParameterGroupResponse_parameterGroup,
    createParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateParameterGroup' smart constructor.
data CreateParameterGroup = CreateParameterGroup'
  { -- | A description of the parameter group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter group to apply to all of the clusters in this
    -- replication group.
    parameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createParameterGroup_description' - A description of the parameter group.
--
-- 'parameterGroupName', 'createParameterGroup_parameterGroupName' - The name of the parameter group to apply to all of the clusters in this
-- replication group.
newCreateParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  CreateParameterGroup
newCreateParameterGroup pParameterGroupName_ =
  CreateParameterGroup'
    { description =
        Prelude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | A description of the parameter group.
createParameterGroup_description :: Lens.Lens' CreateParameterGroup (Prelude.Maybe Prelude.Text)
createParameterGroup_description = Lens.lens (\CreateParameterGroup' {description} -> description) (\s@CreateParameterGroup' {} a -> s {description = a} :: CreateParameterGroup)

-- | The name of the parameter group to apply to all of the clusters in this
-- replication group.
createParameterGroup_parameterGroupName :: Lens.Lens' CreateParameterGroup Prelude.Text
createParameterGroup_parameterGroupName = Lens.lens (\CreateParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@CreateParameterGroup' {} a -> s {parameterGroupName = a} :: CreateParameterGroup)

instance Core.AWSRequest CreateParameterGroup where
  type
    AWSResponse CreateParameterGroup =
      CreateParameterGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateParameterGroupResponse'
            Prelude.<$> (x Data..?> "ParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateParameterGroup where
  hashWithSalt _salt CreateParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData CreateParameterGroup where
  rnf CreateParameterGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameterGroupName

instance Data.ToHeaders CreateParameterGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDAXV3.CreateParameterGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateParameterGroup where
  toJSON CreateParameterGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("ParameterGroupName" Data..= parameterGroupName)
          ]
      )

instance Data.ToPath CreateParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateParameterGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateParameterGroupResponse' smart constructor.
data CreateParameterGroupResponse = CreateParameterGroupResponse'
  { -- | Represents the output of a /CreateParameterGroup/ action.
    parameterGroup :: Prelude.Maybe ParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroup', 'createParameterGroupResponse_parameterGroup' - Represents the output of a /CreateParameterGroup/ action.
--
-- 'httpStatus', 'createParameterGroupResponse_httpStatus' - The response's http status code.
newCreateParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateParameterGroupResponse
newCreateParameterGroupResponse pHttpStatus_ =
  CreateParameterGroupResponse'
    { parameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the output of a /CreateParameterGroup/ action.
createParameterGroupResponse_parameterGroup :: Lens.Lens' CreateParameterGroupResponse (Prelude.Maybe ParameterGroup)
createParameterGroupResponse_parameterGroup = Lens.lens (\CreateParameterGroupResponse' {parameterGroup} -> parameterGroup) (\s@CreateParameterGroupResponse' {} a -> s {parameterGroup = a} :: CreateParameterGroupResponse)

-- | The response's http status code.
createParameterGroupResponse_httpStatus :: Lens.Lens' CreateParameterGroupResponse Prelude.Int
createParameterGroupResponse_httpStatus = Lens.lens (\CreateParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CreateParameterGroupResponse' {} a -> s {httpStatus = a} :: CreateParameterGroupResponse)

instance Prelude.NFData CreateParameterGroupResponse where
  rnf CreateParameterGroupResponse' {..} =
    Prelude.rnf parameterGroup
      `Prelude.seq` Prelude.rnf httpStatus

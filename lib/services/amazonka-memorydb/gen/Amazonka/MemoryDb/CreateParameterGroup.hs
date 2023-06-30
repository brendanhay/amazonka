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
-- Module      : Amazonka.MemoryDb.CreateParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new MemoryDB parameter group. A parameter group is a
-- collection of parameters and their values that are applied to all of the
-- nodes in any cluster. For more information, see
-- <https://docs.aws.amazon.com/MemoryDB/latest/devguide/parametergroups.html Configuring engine parameters using parameter groups>.
module Amazonka.MemoryDb.CreateParameterGroup
  ( -- * Creating a Request
    CreateParameterGroup (..),
    newCreateParameterGroup,

    -- * Request Lenses
    createParameterGroup_description,
    createParameterGroup_tags,
    createParameterGroup_parameterGroupName,
    createParameterGroup_family,

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
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateParameterGroup' smart constructor.
data CreateParameterGroup = CreateParameterGroup'
  { -- | An optional description of the parameter group.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the parameter group.
    parameterGroupName :: Prelude.Text,
    -- | The name of the parameter group family that the parameter group can be
    -- used with.
    family :: Prelude.Text
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
-- 'description', 'createParameterGroup_description' - An optional description of the parameter group.
--
-- 'tags', 'createParameterGroup_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'parameterGroupName', 'createParameterGroup_parameterGroupName' - The name of the parameter group.
--
-- 'family', 'createParameterGroup_family' - The name of the parameter group family that the parameter group can be
-- used with.
newCreateParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  -- | 'family'
  Prelude.Text ->
  CreateParameterGroup
newCreateParameterGroup pParameterGroupName_ pFamily_ =
  CreateParameterGroup'
    { description =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      parameterGroupName = pParameterGroupName_,
      family = pFamily_
    }

-- | An optional description of the parameter group.
createParameterGroup_description :: Lens.Lens' CreateParameterGroup (Prelude.Maybe Prelude.Text)
createParameterGroup_description = Lens.lens (\CreateParameterGroup' {description} -> description) (\s@CreateParameterGroup' {} a -> s {description = a} :: CreateParameterGroup)

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
createParameterGroup_tags :: Lens.Lens' CreateParameterGroup (Prelude.Maybe [Tag])
createParameterGroup_tags = Lens.lens (\CreateParameterGroup' {tags} -> tags) (\s@CreateParameterGroup' {} a -> s {tags = a} :: CreateParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the parameter group.
createParameterGroup_parameterGroupName :: Lens.Lens' CreateParameterGroup Prelude.Text
createParameterGroup_parameterGroupName = Lens.lens (\CreateParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@CreateParameterGroup' {} a -> s {parameterGroupName = a} :: CreateParameterGroup)

-- | The name of the parameter group family that the parameter group can be
-- used with.
createParameterGroup_family :: Lens.Lens' CreateParameterGroup Prelude.Text
createParameterGroup_family = Lens.lens (\CreateParameterGroup' {family} -> family) (\s@CreateParameterGroup' {} a -> s {family = a} :: CreateParameterGroup)

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
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` family

instance Prelude.NFData CreateParameterGroup where
  rnf CreateParameterGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf family

instance Data.ToHeaders CreateParameterGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.CreateParameterGroup" ::
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
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ParameterGroupName" Data..= parameterGroupName),
            Prelude.Just ("Family" Data..= family)
          ]
      )

instance Data.ToPath CreateParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateParameterGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateParameterGroupResponse' smart constructor.
data CreateParameterGroupResponse = CreateParameterGroupResponse'
  { -- | The newly-created parameter group.
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
-- 'parameterGroup', 'createParameterGroupResponse_parameterGroup' - The newly-created parameter group.
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

-- | The newly-created parameter group.
createParameterGroupResponse_parameterGroup :: Lens.Lens' CreateParameterGroupResponse (Prelude.Maybe ParameterGroup)
createParameterGroupResponse_parameterGroup = Lens.lens (\CreateParameterGroupResponse' {parameterGroup} -> parameterGroup) (\s@CreateParameterGroupResponse' {} a -> s {parameterGroup = a} :: CreateParameterGroupResponse)

-- | The response's http status code.
createParameterGroupResponse_httpStatus :: Lens.Lens' CreateParameterGroupResponse Prelude.Int
createParameterGroupResponse_httpStatus = Lens.lens (\CreateParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CreateParameterGroupResponse' {} a -> s {httpStatus = a} :: CreateParameterGroupResponse)

instance Prelude.NFData CreateParameterGroupResponse where
  rnf CreateParameterGroupResponse' {..} =
    Prelude.rnf parameterGroup
      `Prelude.seq` Prelude.rnf httpStatus

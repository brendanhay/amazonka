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
-- Module      : Amazonka.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new option group. You can create up to 20 option groups.
--
-- This command doesn\'t apply to RDS Custom.
module Amazonka.RDS.CreateOptionGroup
  ( -- * Creating a Request
    CreateOptionGroup (..),
    newCreateOptionGroup,

    -- * Request Lenses
    createOptionGroup_tags,
    createOptionGroup_optionGroupName,
    createOptionGroup_engineName,
    createOptionGroup_majorEngineVersion,
    createOptionGroup_optionGroupDescription,

    -- * Destructuring the Response
    CreateOptionGroupResponse (..),
    newCreateOptionGroupResponse,

    -- * Response Lenses
    createOptionGroupResponse_optionGroup,
    createOptionGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateOptionGroup' smart constructor.
data CreateOptionGroup = CreateOptionGroup'
  { -- | Tags to assign to the option group.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the name of the option group to be created.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @myoptiongroup@
    optionGroupName :: Prelude.Text,
    -- | Specifies the name of the engine that this option group should be
    -- associated with.
    --
    -- Valid Values:
    --
    -- -   @mariadb@
    --
    -- -   @mysql@
    --
    -- -   @oracle-ee@
    --
    -- -   @oracle-ee-cdb@
    --
    -- -   @oracle-se2@
    --
    -- -   @oracle-se2-cdb@
    --
    -- -   @postgres@
    --
    -- -   @sqlserver-ee@
    --
    -- -   @sqlserver-se@
    --
    -- -   @sqlserver-ex@
    --
    -- -   @sqlserver-web@
    engineName :: Prelude.Text,
    -- | Specifies the major version of the engine that this option group should
    -- be associated with.
    majorEngineVersion :: Prelude.Text,
    -- | The description of the option group.
    optionGroupDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOptionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createOptionGroup_tags' - Tags to assign to the option group.
--
-- 'optionGroupName', 'createOptionGroup_optionGroupName' - Specifies the name of the option group to be created.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @myoptiongroup@
--
-- 'engineName', 'createOptionGroup_engineName' - Specifies the name of the engine that this option group should be
-- associated with.
--
-- Valid Values:
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-ee-cdb@
--
-- -   @oracle-se2@
--
-- -   @oracle-se2-cdb@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
--
-- 'majorEngineVersion', 'createOptionGroup_majorEngineVersion' - Specifies the major version of the engine that this option group should
-- be associated with.
--
-- 'optionGroupDescription', 'createOptionGroup_optionGroupDescription' - The description of the option group.
newCreateOptionGroup ::
  -- | 'optionGroupName'
  Prelude.Text ->
  -- | 'engineName'
  Prelude.Text ->
  -- | 'majorEngineVersion'
  Prelude.Text ->
  -- | 'optionGroupDescription'
  Prelude.Text ->
  CreateOptionGroup
newCreateOptionGroup
  pOptionGroupName_
  pEngineName_
  pMajorEngineVersion_
  pOptionGroupDescription_ =
    CreateOptionGroup'
      { tags = Prelude.Nothing,
        optionGroupName = pOptionGroupName_,
        engineName = pEngineName_,
        majorEngineVersion = pMajorEngineVersion_,
        optionGroupDescription = pOptionGroupDescription_
      }

-- | Tags to assign to the option group.
createOptionGroup_tags :: Lens.Lens' CreateOptionGroup (Prelude.Maybe [Tag])
createOptionGroup_tags = Lens.lens (\CreateOptionGroup' {tags} -> tags) (\s@CreateOptionGroup' {} a -> s {tags = a} :: CreateOptionGroup) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the option group to be created.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @myoptiongroup@
createOptionGroup_optionGroupName :: Lens.Lens' CreateOptionGroup Prelude.Text
createOptionGroup_optionGroupName = Lens.lens (\CreateOptionGroup' {optionGroupName} -> optionGroupName) (\s@CreateOptionGroup' {} a -> s {optionGroupName = a} :: CreateOptionGroup)

-- | Specifies the name of the engine that this option group should be
-- associated with.
--
-- Valid Values:
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-ee-cdb@
--
-- -   @oracle-se2@
--
-- -   @oracle-se2-cdb@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
createOptionGroup_engineName :: Lens.Lens' CreateOptionGroup Prelude.Text
createOptionGroup_engineName = Lens.lens (\CreateOptionGroup' {engineName} -> engineName) (\s@CreateOptionGroup' {} a -> s {engineName = a} :: CreateOptionGroup)

-- | Specifies the major version of the engine that this option group should
-- be associated with.
createOptionGroup_majorEngineVersion :: Lens.Lens' CreateOptionGroup Prelude.Text
createOptionGroup_majorEngineVersion = Lens.lens (\CreateOptionGroup' {majorEngineVersion} -> majorEngineVersion) (\s@CreateOptionGroup' {} a -> s {majorEngineVersion = a} :: CreateOptionGroup)

-- | The description of the option group.
createOptionGroup_optionGroupDescription :: Lens.Lens' CreateOptionGroup Prelude.Text
createOptionGroup_optionGroupDescription = Lens.lens (\CreateOptionGroup' {optionGroupDescription} -> optionGroupDescription) (\s@CreateOptionGroup' {} a -> s {optionGroupDescription = a} :: CreateOptionGroup)

instance Core.AWSRequest CreateOptionGroup where
  type
    AWSResponse CreateOptionGroup =
      CreateOptionGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateOptionGroupResult"
      ( \s h x ->
          CreateOptionGroupResponse'
            Prelude.<$> (x Core..@? "OptionGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOptionGroup where
  hashWithSalt _salt CreateOptionGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` engineName
      `Prelude.hashWithSalt` majorEngineVersion
      `Prelude.hashWithSalt` optionGroupDescription

instance Prelude.NFData CreateOptionGroup where
  rnf CreateOptionGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf engineName
      `Prelude.seq` Prelude.rnf majorEngineVersion
      `Prelude.seq` Prelude.rnf optionGroupDescription

instance Core.ToHeaders CreateOptionGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateOptionGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateOptionGroup where
  toQuery CreateOptionGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateOptionGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "OptionGroupName" Core.=: optionGroupName,
        "EngineName" Core.=: engineName,
        "MajorEngineVersion" Core.=: majorEngineVersion,
        "OptionGroupDescription"
          Core.=: optionGroupDescription
      ]

-- | /See:/ 'newCreateOptionGroupResponse' smart constructor.
data CreateOptionGroupResponse = CreateOptionGroupResponse'
  { optionGroup :: Prelude.Maybe OptionGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOptionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionGroup', 'createOptionGroupResponse_optionGroup' - Undocumented member.
--
-- 'httpStatus', 'createOptionGroupResponse_httpStatus' - The response's http status code.
newCreateOptionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOptionGroupResponse
newCreateOptionGroupResponse pHttpStatus_ =
  CreateOptionGroupResponse'
    { optionGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createOptionGroupResponse_optionGroup :: Lens.Lens' CreateOptionGroupResponse (Prelude.Maybe OptionGroup)
createOptionGroupResponse_optionGroup = Lens.lens (\CreateOptionGroupResponse' {optionGroup} -> optionGroup) (\s@CreateOptionGroupResponse' {} a -> s {optionGroup = a} :: CreateOptionGroupResponse)

-- | The response's http status code.
createOptionGroupResponse_httpStatus :: Lens.Lens' CreateOptionGroupResponse Prelude.Int
createOptionGroupResponse_httpStatus = Lens.lens (\CreateOptionGroupResponse' {httpStatus} -> httpStatus) (\s@CreateOptionGroupResponse' {} a -> s {httpStatus = a} :: CreateOptionGroupResponse)

instance Prelude.NFData CreateOptionGroupResponse where
  rnf CreateOptionGroupResponse' {..} =
    Prelude.rnf optionGroup
      `Prelude.seq` Prelude.rnf httpStatus

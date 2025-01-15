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
-- Module      : Amazonka.RDS.CopyOptionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified option group.
module Amazonka.RDS.CopyOptionGroup
  ( -- * Creating a Request
    CopyOptionGroup (..),
    newCopyOptionGroup,

    -- * Request Lenses
    copyOptionGroup_tags,
    copyOptionGroup_sourceOptionGroupIdentifier,
    copyOptionGroup_targetOptionGroupIdentifier,
    copyOptionGroup_targetOptionGroupDescription,

    -- * Destructuring the Response
    CopyOptionGroupResponse (..),
    newCopyOptionGroupResponse,

    -- * Response Lenses
    copyOptionGroupResponse_optionGroup,
    copyOptionGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCopyOptionGroup' smart constructor.
data CopyOptionGroup = CopyOptionGroup'
  { tags :: Prelude.Maybe [Tag],
    -- | The identifier for the source option group.
    --
    -- Constraints:
    --
    -- -   Must specify a valid option group.
    sourceOptionGroupIdentifier :: Prelude.Text,
    -- | The identifier for the copied option group.
    --
    -- Constraints:
    --
    -- -   Can\'t be null, empty, or blank
    --
    -- -   Must contain from 1 to 255 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-option-group@
    targetOptionGroupIdentifier :: Prelude.Text,
    -- | The description for the copied option group.
    targetOptionGroupDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyOptionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'copyOptionGroup_tags' - Undocumented member.
--
-- 'sourceOptionGroupIdentifier', 'copyOptionGroup_sourceOptionGroupIdentifier' - The identifier for the source option group.
--
-- Constraints:
--
-- -   Must specify a valid option group.
--
-- 'targetOptionGroupIdentifier', 'copyOptionGroup_targetOptionGroupIdentifier' - The identifier for the copied option group.
--
-- Constraints:
--
-- -   Can\'t be null, empty, or blank
--
-- -   Must contain from 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-option-group@
--
-- 'targetOptionGroupDescription', 'copyOptionGroup_targetOptionGroupDescription' - The description for the copied option group.
newCopyOptionGroup ::
  -- | 'sourceOptionGroupIdentifier'
  Prelude.Text ->
  -- | 'targetOptionGroupIdentifier'
  Prelude.Text ->
  -- | 'targetOptionGroupDescription'
  Prelude.Text ->
  CopyOptionGroup
newCopyOptionGroup
  pSourceOptionGroupIdentifier_
  pTargetOptionGroupIdentifier_
  pTargetOptionGroupDescription_ =
    CopyOptionGroup'
      { tags = Prelude.Nothing,
        sourceOptionGroupIdentifier =
          pSourceOptionGroupIdentifier_,
        targetOptionGroupIdentifier =
          pTargetOptionGroupIdentifier_,
        targetOptionGroupDescription =
          pTargetOptionGroupDescription_
      }

-- | Undocumented member.
copyOptionGroup_tags :: Lens.Lens' CopyOptionGroup (Prelude.Maybe [Tag])
copyOptionGroup_tags = Lens.lens (\CopyOptionGroup' {tags} -> tags) (\s@CopyOptionGroup' {} a -> s {tags = a} :: CopyOptionGroup) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the source option group.
--
-- Constraints:
--
-- -   Must specify a valid option group.
copyOptionGroup_sourceOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Prelude.Text
copyOptionGroup_sourceOptionGroupIdentifier = Lens.lens (\CopyOptionGroup' {sourceOptionGroupIdentifier} -> sourceOptionGroupIdentifier) (\s@CopyOptionGroup' {} a -> s {sourceOptionGroupIdentifier = a} :: CopyOptionGroup)

-- | The identifier for the copied option group.
--
-- Constraints:
--
-- -   Can\'t be null, empty, or blank
--
-- -   Must contain from 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-option-group@
copyOptionGroup_targetOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Prelude.Text
copyOptionGroup_targetOptionGroupIdentifier = Lens.lens (\CopyOptionGroup' {targetOptionGroupIdentifier} -> targetOptionGroupIdentifier) (\s@CopyOptionGroup' {} a -> s {targetOptionGroupIdentifier = a} :: CopyOptionGroup)

-- | The description for the copied option group.
copyOptionGroup_targetOptionGroupDescription :: Lens.Lens' CopyOptionGroup Prelude.Text
copyOptionGroup_targetOptionGroupDescription = Lens.lens (\CopyOptionGroup' {targetOptionGroupDescription} -> targetOptionGroupDescription) (\s@CopyOptionGroup' {} a -> s {targetOptionGroupDescription = a} :: CopyOptionGroup)

instance Core.AWSRequest CopyOptionGroup where
  type
    AWSResponse CopyOptionGroup =
      CopyOptionGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CopyOptionGroupResult"
      ( \s h x ->
          CopyOptionGroupResponse'
            Prelude.<$> (x Data..@? "OptionGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyOptionGroup where
  hashWithSalt _salt CopyOptionGroup' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceOptionGroupIdentifier
      `Prelude.hashWithSalt` targetOptionGroupIdentifier
      `Prelude.hashWithSalt` targetOptionGroupDescription

instance Prelude.NFData CopyOptionGroup where
  rnf CopyOptionGroup' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf sourceOptionGroupIdentifier `Prelude.seq`
        Prelude.rnf targetOptionGroupIdentifier `Prelude.seq`
          Prelude.rnf targetOptionGroupDescription

instance Data.ToHeaders CopyOptionGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopyOptionGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyOptionGroup where
  toQuery CopyOptionGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopyOptionGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "SourceOptionGroupIdentifier"
          Data.=: sourceOptionGroupIdentifier,
        "TargetOptionGroupIdentifier"
          Data.=: targetOptionGroupIdentifier,
        "TargetOptionGroupDescription"
          Data.=: targetOptionGroupDescription
      ]

-- | /See:/ 'newCopyOptionGroupResponse' smart constructor.
data CopyOptionGroupResponse = CopyOptionGroupResponse'
  { optionGroup :: Prelude.Maybe OptionGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyOptionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionGroup', 'copyOptionGroupResponse_optionGroup' - Undocumented member.
--
-- 'httpStatus', 'copyOptionGroupResponse_httpStatus' - The response's http status code.
newCopyOptionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyOptionGroupResponse
newCopyOptionGroupResponse pHttpStatus_ =
  CopyOptionGroupResponse'
    { optionGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyOptionGroupResponse_optionGroup :: Lens.Lens' CopyOptionGroupResponse (Prelude.Maybe OptionGroup)
copyOptionGroupResponse_optionGroup = Lens.lens (\CopyOptionGroupResponse' {optionGroup} -> optionGroup) (\s@CopyOptionGroupResponse' {} a -> s {optionGroup = a} :: CopyOptionGroupResponse)

-- | The response's http status code.
copyOptionGroupResponse_httpStatus :: Lens.Lens' CopyOptionGroupResponse Prelude.Int
copyOptionGroupResponse_httpStatus = Lens.lens (\CopyOptionGroupResponse' {httpStatus} -> httpStatus) (\s@CopyOptionGroupResponse' {} a -> s {httpStatus = a} :: CopyOptionGroupResponse)

instance Prelude.NFData CopyOptionGroupResponse where
  rnf CopyOptionGroupResponse' {..} =
    Prelude.rnf optionGroup `Prelude.seq`
      Prelude.rnf httpStatus

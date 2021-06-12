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
-- Module      : Network.AWS.RDS.CopyOptionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified option group.
module Network.AWS.RDS.CopyOptionGroup
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCopyOptionGroup' smart constructor.
data CopyOptionGroup = CopyOptionGroup'
  { tags :: Core.Maybe [Tag],
    -- | The identifier for the source option group.
    --
    -- Constraints:
    --
    -- -   Must specify a valid option group.
    sourceOptionGroupIdentifier :: Core.Text,
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
    targetOptionGroupIdentifier :: Core.Text,
    -- | The description for the copied option group.
    targetOptionGroupDescription :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'targetOptionGroupIdentifier'
  Core.Text ->
  -- | 'targetOptionGroupDescription'
  Core.Text ->
  CopyOptionGroup
newCopyOptionGroup
  pSourceOptionGroupIdentifier_
  pTargetOptionGroupIdentifier_
  pTargetOptionGroupDescription_ =
    CopyOptionGroup'
      { tags = Core.Nothing,
        sourceOptionGroupIdentifier =
          pSourceOptionGroupIdentifier_,
        targetOptionGroupIdentifier =
          pTargetOptionGroupIdentifier_,
        targetOptionGroupDescription =
          pTargetOptionGroupDescription_
      }

-- | Undocumented member.
copyOptionGroup_tags :: Lens.Lens' CopyOptionGroup (Core.Maybe [Tag])
copyOptionGroup_tags = Lens.lens (\CopyOptionGroup' {tags} -> tags) (\s@CopyOptionGroup' {} a -> s {tags = a} :: CopyOptionGroup) Core.. Lens.mapping Lens._Coerce

-- | The identifier for the source option group.
--
-- Constraints:
--
-- -   Must specify a valid option group.
copyOptionGroup_sourceOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Core.Text
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
copyOptionGroup_targetOptionGroupIdentifier :: Lens.Lens' CopyOptionGroup Core.Text
copyOptionGroup_targetOptionGroupIdentifier = Lens.lens (\CopyOptionGroup' {targetOptionGroupIdentifier} -> targetOptionGroupIdentifier) (\s@CopyOptionGroup' {} a -> s {targetOptionGroupIdentifier = a} :: CopyOptionGroup)

-- | The description for the copied option group.
copyOptionGroup_targetOptionGroupDescription :: Lens.Lens' CopyOptionGroup Core.Text
copyOptionGroup_targetOptionGroupDescription = Lens.lens (\CopyOptionGroup' {targetOptionGroupDescription} -> targetOptionGroupDescription) (\s@CopyOptionGroup' {} a -> s {targetOptionGroupDescription = a} :: CopyOptionGroup)

instance Core.AWSRequest CopyOptionGroup where
  type
    AWSResponse CopyOptionGroup =
      CopyOptionGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CopyOptionGroupResult"
      ( \s h x ->
          CopyOptionGroupResponse'
            Core.<$> (x Core..@? "OptionGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CopyOptionGroup

instance Core.NFData CopyOptionGroup

instance Core.ToHeaders CopyOptionGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CopyOptionGroup where
  toPath = Core.const "/"

instance Core.ToQuery CopyOptionGroup where
  toQuery CopyOptionGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CopyOptionGroup" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "SourceOptionGroupIdentifier"
          Core.=: sourceOptionGroupIdentifier,
        "TargetOptionGroupIdentifier"
          Core.=: targetOptionGroupIdentifier,
        "TargetOptionGroupDescription"
          Core.=: targetOptionGroupDescription
      ]

-- | /See:/ 'newCopyOptionGroupResponse' smart constructor.
data CopyOptionGroupResponse = CopyOptionGroupResponse'
  { optionGroup :: Core.Maybe OptionGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CopyOptionGroupResponse
newCopyOptionGroupResponse pHttpStatus_ =
  CopyOptionGroupResponse'
    { optionGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyOptionGroupResponse_optionGroup :: Lens.Lens' CopyOptionGroupResponse (Core.Maybe OptionGroup)
copyOptionGroupResponse_optionGroup = Lens.lens (\CopyOptionGroupResponse' {optionGroup} -> optionGroup) (\s@CopyOptionGroupResponse' {} a -> s {optionGroup = a} :: CopyOptionGroupResponse)

-- | The response's http status code.
copyOptionGroupResponse_httpStatus :: Lens.Lens' CopyOptionGroupResponse Core.Int
copyOptionGroupResponse_httpStatus = Lens.lens (\CopyOptionGroupResponse' {httpStatus} -> httpStatus) (\s@CopyOptionGroupResponse' {} a -> s {httpStatus = a} :: CopyOptionGroupResponse)

instance Core.NFData CopyOptionGroupResponse

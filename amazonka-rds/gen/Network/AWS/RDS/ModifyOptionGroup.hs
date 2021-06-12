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
-- Module      : Network.AWS.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing option group.
module Network.AWS.RDS.ModifyOptionGroup
  ( -- * Creating a Request
    ModifyOptionGroup (..),
    newModifyOptionGroup,

    -- * Request Lenses
    modifyOptionGroup_optionsToInclude,
    modifyOptionGroup_optionsToRemove,
    modifyOptionGroup_applyImmediately,
    modifyOptionGroup_optionGroupName,

    -- * Destructuring the Response
    ModifyOptionGroupResponse (..),
    newModifyOptionGroupResponse,

    -- * Response Lenses
    modifyOptionGroupResponse_optionGroup,
    modifyOptionGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyOptionGroup' smart constructor.
data ModifyOptionGroup = ModifyOptionGroup'
  { -- | Options in this list are added to the option group or, if already
    -- present, the specified configuration is used to update the existing
    -- configuration.
    optionsToInclude :: Core.Maybe [OptionConfiguration],
    -- | Options in this list are removed from the option group.
    optionsToRemove :: Core.Maybe [Core.Text],
    -- | A value that indicates whether to apply the change immediately or during
    -- the next maintenance window for each instance associated with the option
    -- group.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | The name of the option group to be modified.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group, and that option group
    -- can\'t be removed from a DB instance once it is associated with a DB
    -- instance
    optionGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyOptionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionsToInclude', 'modifyOptionGroup_optionsToInclude' - Options in this list are added to the option group or, if already
-- present, the specified configuration is used to update the existing
-- configuration.
--
-- 'optionsToRemove', 'modifyOptionGroup_optionsToRemove' - Options in this list are removed from the option group.
--
-- 'applyImmediately', 'modifyOptionGroup_applyImmediately' - A value that indicates whether to apply the change immediately or during
-- the next maintenance window for each instance associated with the option
-- group.
--
-- 'optionGroupName', 'modifyOptionGroup_optionGroupName' - The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
newModifyOptionGroup ::
  -- | 'optionGroupName'
  Core.Text ->
  ModifyOptionGroup
newModifyOptionGroup pOptionGroupName_ =
  ModifyOptionGroup'
    { optionsToInclude = Core.Nothing,
      optionsToRemove = Core.Nothing,
      applyImmediately = Core.Nothing,
      optionGroupName = pOptionGroupName_
    }

-- | Options in this list are added to the option group or, if already
-- present, the specified configuration is used to update the existing
-- configuration.
modifyOptionGroup_optionsToInclude :: Lens.Lens' ModifyOptionGroup (Core.Maybe [OptionConfiguration])
modifyOptionGroup_optionsToInclude = Lens.lens (\ModifyOptionGroup' {optionsToInclude} -> optionsToInclude) (\s@ModifyOptionGroup' {} a -> s {optionsToInclude = a} :: ModifyOptionGroup) Core.. Lens.mapping Lens._Coerce

-- | Options in this list are removed from the option group.
modifyOptionGroup_optionsToRemove :: Lens.Lens' ModifyOptionGroup (Core.Maybe [Core.Text])
modifyOptionGroup_optionsToRemove = Lens.lens (\ModifyOptionGroup' {optionsToRemove} -> optionsToRemove) (\s@ModifyOptionGroup' {} a -> s {optionsToRemove = a} :: ModifyOptionGroup) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates whether to apply the change immediately or during
-- the next maintenance window for each instance associated with the option
-- group.
modifyOptionGroup_applyImmediately :: Lens.Lens' ModifyOptionGroup (Core.Maybe Core.Bool)
modifyOptionGroup_applyImmediately = Lens.lens (\ModifyOptionGroup' {applyImmediately} -> applyImmediately) (\s@ModifyOptionGroup' {} a -> s {applyImmediately = a} :: ModifyOptionGroup)

-- | The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
modifyOptionGroup_optionGroupName :: Lens.Lens' ModifyOptionGroup Core.Text
modifyOptionGroup_optionGroupName = Lens.lens (\ModifyOptionGroup' {optionGroupName} -> optionGroupName) (\s@ModifyOptionGroup' {} a -> s {optionGroupName = a} :: ModifyOptionGroup)

instance Core.AWSRequest ModifyOptionGroup where
  type
    AWSResponse ModifyOptionGroup =
      ModifyOptionGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyOptionGroupResult"
      ( \s h x ->
          ModifyOptionGroupResponse'
            Core.<$> (x Core..@? "OptionGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyOptionGroup

instance Core.NFData ModifyOptionGroup

instance Core.ToHeaders ModifyOptionGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyOptionGroup where
  toPath = Core.const "/"

instance Core.ToQuery ModifyOptionGroup where
  toQuery ModifyOptionGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyOptionGroup" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "OptionsToInclude"
          Core.=: Core.toQuery
            ( Core.toQueryList "OptionConfiguration"
                Core.<$> optionsToInclude
            ),
        "OptionsToRemove"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> optionsToRemove),
        "ApplyImmediately" Core.=: applyImmediately,
        "OptionGroupName" Core.=: optionGroupName
      ]

-- | /See:/ 'newModifyOptionGroupResponse' smart constructor.
data ModifyOptionGroupResponse = ModifyOptionGroupResponse'
  { optionGroup :: Core.Maybe OptionGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyOptionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionGroup', 'modifyOptionGroupResponse_optionGroup' - Undocumented member.
--
-- 'httpStatus', 'modifyOptionGroupResponse_httpStatus' - The response's http status code.
newModifyOptionGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyOptionGroupResponse
newModifyOptionGroupResponse pHttpStatus_ =
  ModifyOptionGroupResponse'
    { optionGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyOptionGroupResponse_optionGroup :: Lens.Lens' ModifyOptionGroupResponse (Core.Maybe OptionGroup)
modifyOptionGroupResponse_optionGroup = Lens.lens (\ModifyOptionGroupResponse' {optionGroup} -> optionGroup) (\s@ModifyOptionGroupResponse' {} a -> s {optionGroup = a} :: ModifyOptionGroupResponse)

-- | The response's http status code.
modifyOptionGroupResponse_httpStatus :: Lens.Lens' ModifyOptionGroupResponse Core.Int
modifyOptionGroupResponse_httpStatus = Lens.lens (\ModifyOptionGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyOptionGroupResponse' {} a -> s {httpStatus = a} :: ModifyOptionGroupResponse)

instance Core.NFData ModifyOptionGroupResponse

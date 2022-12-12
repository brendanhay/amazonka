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
-- Module      : Amazonka.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing option group.
module Amazonka.RDS.ModifyOptionGroup
  ( -- * Creating a Request
    ModifyOptionGroup (..),
    newModifyOptionGroup,

    -- * Request Lenses
    modifyOptionGroup_applyImmediately,
    modifyOptionGroup_optionsToInclude,
    modifyOptionGroup_optionsToRemove,
    modifyOptionGroup_optionGroupName,

    -- * Destructuring the Response
    ModifyOptionGroupResponse (..),
    newModifyOptionGroupResponse,

    -- * Response Lenses
    modifyOptionGroupResponse_optionGroup,
    modifyOptionGroupResponse_httpStatus,
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
-- /See:/ 'newModifyOptionGroup' smart constructor.
data ModifyOptionGroup = ModifyOptionGroup'
  { -- | A value that indicates whether to apply the change immediately or during
    -- the next maintenance window for each instance associated with the option
    -- group.
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | Options in this list are added to the option group or, if already
    -- present, the specified configuration is used to update the existing
    -- configuration.
    optionsToInclude :: Prelude.Maybe [OptionConfiguration],
    -- | Options in this list are removed from the option group.
    optionsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The name of the option group to be modified.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group, and that option group
    -- can\'t be removed from a DB instance once it is associated with a DB
    -- instance
    optionGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyOptionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyImmediately', 'modifyOptionGroup_applyImmediately' - A value that indicates whether to apply the change immediately or during
-- the next maintenance window for each instance associated with the option
-- group.
--
-- 'optionsToInclude', 'modifyOptionGroup_optionsToInclude' - Options in this list are added to the option group or, if already
-- present, the specified configuration is used to update the existing
-- configuration.
--
-- 'optionsToRemove', 'modifyOptionGroup_optionsToRemove' - Options in this list are removed from the option group.
--
-- 'optionGroupName', 'modifyOptionGroup_optionGroupName' - The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
newModifyOptionGroup ::
  -- | 'optionGroupName'
  Prelude.Text ->
  ModifyOptionGroup
newModifyOptionGroup pOptionGroupName_ =
  ModifyOptionGroup'
    { applyImmediately =
        Prelude.Nothing,
      optionsToInclude = Prelude.Nothing,
      optionsToRemove = Prelude.Nothing,
      optionGroupName = pOptionGroupName_
    }

-- | A value that indicates whether to apply the change immediately or during
-- the next maintenance window for each instance associated with the option
-- group.
modifyOptionGroup_applyImmediately :: Lens.Lens' ModifyOptionGroup (Prelude.Maybe Prelude.Bool)
modifyOptionGroup_applyImmediately = Lens.lens (\ModifyOptionGroup' {applyImmediately} -> applyImmediately) (\s@ModifyOptionGroup' {} a -> s {applyImmediately = a} :: ModifyOptionGroup)

-- | Options in this list are added to the option group or, if already
-- present, the specified configuration is used to update the existing
-- configuration.
modifyOptionGroup_optionsToInclude :: Lens.Lens' ModifyOptionGroup (Prelude.Maybe [OptionConfiguration])
modifyOptionGroup_optionsToInclude = Lens.lens (\ModifyOptionGroup' {optionsToInclude} -> optionsToInclude) (\s@ModifyOptionGroup' {} a -> s {optionsToInclude = a} :: ModifyOptionGroup) Prelude.. Lens.mapping Lens.coerced

-- | Options in this list are removed from the option group.
modifyOptionGroup_optionsToRemove :: Lens.Lens' ModifyOptionGroup (Prelude.Maybe [Prelude.Text])
modifyOptionGroup_optionsToRemove = Lens.lens (\ModifyOptionGroup' {optionsToRemove} -> optionsToRemove) (\s@ModifyOptionGroup' {} a -> s {optionsToRemove = a} :: ModifyOptionGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
modifyOptionGroup_optionGroupName :: Lens.Lens' ModifyOptionGroup Prelude.Text
modifyOptionGroup_optionGroupName = Lens.lens (\ModifyOptionGroup' {optionGroupName} -> optionGroupName) (\s@ModifyOptionGroup' {} a -> s {optionGroupName = a} :: ModifyOptionGroup)

instance Core.AWSRequest ModifyOptionGroup where
  type
    AWSResponse ModifyOptionGroup =
      ModifyOptionGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyOptionGroupResult"
      ( \s h x ->
          ModifyOptionGroupResponse'
            Prelude.<$> (x Data..@? "OptionGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyOptionGroup where
  hashWithSalt _salt ModifyOptionGroup' {..} =
    _salt `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` optionsToInclude
      `Prelude.hashWithSalt` optionsToRemove
      `Prelude.hashWithSalt` optionGroupName

instance Prelude.NFData ModifyOptionGroup where
  rnf ModifyOptionGroup' {..} =
    Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf optionsToInclude
      `Prelude.seq` Prelude.rnf optionsToRemove
      `Prelude.seq` Prelude.rnf optionGroupName

instance Data.ToHeaders ModifyOptionGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyOptionGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyOptionGroup where
  toQuery ModifyOptionGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyOptionGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ApplyImmediately" Data.=: applyImmediately,
        "OptionsToInclude"
          Data.=: Data.toQuery
            ( Data.toQueryList "OptionConfiguration"
                Prelude.<$> optionsToInclude
            ),
        "OptionsToRemove"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> optionsToRemove
            ),
        "OptionGroupName" Data.=: optionGroupName
      ]

-- | /See:/ 'newModifyOptionGroupResponse' smart constructor.
data ModifyOptionGroupResponse = ModifyOptionGroupResponse'
  { optionGroup :: Prelude.Maybe OptionGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyOptionGroupResponse
newModifyOptionGroupResponse pHttpStatus_ =
  ModifyOptionGroupResponse'
    { optionGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyOptionGroupResponse_optionGroup :: Lens.Lens' ModifyOptionGroupResponse (Prelude.Maybe OptionGroup)
modifyOptionGroupResponse_optionGroup = Lens.lens (\ModifyOptionGroupResponse' {optionGroup} -> optionGroup) (\s@ModifyOptionGroupResponse' {} a -> s {optionGroup = a} :: ModifyOptionGroupResponse)

-- | The response's http status code.
modifyOptionGroupResponse_httpStatus :: Lens.Lens' ModifyOptionGroupResponse Prelude.Int
modifyOptionGroupResponse_httpStatus = Lens.lens (\ModifyOptionGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyOptionGroupResponse' {} a -> s {httpStatus = a} :: ModifyOptionGroupResponse)

instance Prelude.NFData ModifyOptionGroupResponse where
  rnf ModifyOptionGroupResponse' {..} =
    Prelude.rnf optionGroup
      `Prelude.seq` Prelude.rnf httpStatus

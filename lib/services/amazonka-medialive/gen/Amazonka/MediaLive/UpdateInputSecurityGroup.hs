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
-- Module      : Amazonka.MediaLive.UpdateInputSecurityGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an Input Security Group\'s Whilelists.
module Amazonka.MediaLive.UpdateInputSecurityGroup
  ( -- * Creating a Request
    UpdateInputSecurityGroup (..),
    newUpdateInputSecurityGroup,

    -- * Request Lenses
    updateInputSecurityGroup_tags,
    updateInputSecurityGroup_whitelistRules,
    updateInputSecurityGroup_inputSecurityGroupId,

    -- * Destructuring the Response
    UpdateInputSecurityGroupResponse (..),
    newUpdateInputSecurityGroupResponse,

    -- * Response Lenses
    updateInputSecurityGroupResponse_securityGroup,
    updateInputSecurityGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to update some combination of the Input Security Group name
-- and the IPv4 CIDRs the Input Security Group should allow.
--
-- /See:/ 'newUpdateInputSecurityGroup' smart constructor.
data UpdateInputSecurityGroup = UpdateInputSecurityGroup'
  { -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | List of IPv4 CIDR addresses to whitelist
    whitelistRules :: Prelude.Maybe [InputWhitelistRuleCidr],
    -- | The id of the Input Security Group to update.
    inputSecurityGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInputSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateInputSecurityGroup_tags' - A collection of key-value pairs.
--
-- 'whitelistRules', 'updateInputSecurityGroup_whitelistRules' - List of IPv4 CIDR addresses to whitelist
--
-- 'inputSecurityGroupId', 'updateInputSecurityGroup_inputSecurityGroupId' - The id of the Input Security Group to update.
newUpdateInputSecurityGroup ::
  -- | 'inputSecurityGroupId'
  Prelude.Text ->
  UpdateInputSecurityGroup
newUpdateInputSecurityGroup pInputSecurityGroupId_ =
  UpdateInputSecurityGroup'
    { tags = Prelude.Nothing,
      whitelistRules = Prelude.Nothing,
      inputSecurityGroupId = pInputSecurityGroupId_
    }

-- | A collection of key-value pairs.
updateInputSecurityGroup_tags :: Lens.Lens' UpdateInputSecurityGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateInputSecurityGroup_tags = Lens.lens (\UpdateInputSecurityGroup' {tags} -> tags) (\s@UpdateInputSecurityGroup' {} a -> s {tags = a} :: UpdateInputSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | List of IPv4 CIDR addresses to whitelist
updateInputSecurityGroup_whitelistRules :: Lens.Lens' UpdateInputSecurityGroup (Prelude.Maybe [InputWhitelistRuleCidr])
updateInputSecurityGroup_whitelistRules = Lens.lens (\UpdateInputSecurityGroup' {whitelistRules} -> whitelistRules) (\s@UpdateInputSecurityGroup' {} a -> s {whitelistRules = a} :: UpdateInputSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | The id of the Input Security Group to update.
updateInputSecurityGroup_inputSecurityGroupId :: Lens.Lens' UpdateInputSecurityGroup Prelude.Text
updateInputSecurityGroup_inputSecurityGroupId = Lens.lens (\UpdateInputSecurityGroup' {inputSecurityGroupId} -> inputSecurityGroupId) (\s@UpdateInputSecurityGroup' {} a -> s {inputSecurityGroupId = a} :: UpdateInputSecurityGroup)

instance Core.AWSRequest UpdateInputSecurityGroup where
  type
    AWSResponse UpdateInputSecurityGroup =
      UpdateInputSecurityGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputSecurityGroupResponse'
            Prelude.<$> (x Core..?> "securityGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateInputSecurityGroup where
  hashWithSalt _salt UpdateInputSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` whitelistRules
      `Prelude.hashWithSalt` inputSecurityGroupId

instance Prelude.NFData UpdateInputSecurityGroup where
  rnf UpdateInputSecurityGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf whitelistRules
      `Prelude.seq` Prelude.rnf inputSecurityGroupId

instance Core.ToHeaders UpdateInputSecurityGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateInputSecurityGroup where
  toJSON UpdateInputSecurityGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("whitelistRules" Core..=)
              Prelude.<$> whitelistRules
          ]
      )

instance Core.ToPath UpdateInputSecurityGroup where
  toPath UpdateInputSecurityGroup' {..} =
    Prelude.mconcat
      [ "/prod/inputSecurityGroups/",
        Core.toBS inputSecurityGroupId
      ]

instance Core.ToQuery UpdateInputSecurityGroup where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for UpdateInputSecurityGroupResponse
--
-- /See:/ 'newUpdateInputSecurityGroupResponse' smart constructor.
data UpdateInputSecurityGroupResponse = UpdateInputSecurityGroupResponse'
  { securityGroup :: Prelude.Maybe InputSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInputSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroup', 'updateInputSecurityGroupResponse_securityGroup' - Undocumented member.
--
-- 'httpStatus', 'updateInputSecurityGroupResponse_httpStatus' - The response's http status code.
newUpdateInputSecurityGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateInputSecurityGroupResponse
newUpdateInputSecurityGroupResponse pHttpStatus_ =
  UpdateInputSecurityGroupResponse'
    { securityGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateInputSecurityGroupResponse_securityGroup :: Lens.Lens' UpdateInputSecurityGroupResponse (Prelude.Maybe InputSecurityGroup)
updateInputSecurityGroupResponse_securityGroup = Lens.lens (\UpdateInputSecurityGroupResponse' {securityGroup} -> securityGroup) (\s@UpdateInputSecurityGroupResponse' {} a -> s {securityGroup = a} :: UpdateInputSecurityGroupResponse)

-- | The response's http status code.
updateInputSecurityGroupResponse_httpStatus :: Lens.Lens' UpdateInputSecurityGroupResponse Prelude.Int
updateInputSecurityGroupResponse_httpStatus = Lens.lens (\UpdateInputSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateInputSecurityGroupResponse' {} a -> s {httpStatus = a} :: UpdateInputSecurityGroupResponse)

instance
  Prelude.NFData
    UpdateInputSecurityGroupResponse
  where
  rnf UpdateInputSecurityGroupResponse' {..} =
    Prelude.rnf securityGroup
      `Prelude.seq` Prelude.rnf httpStatus

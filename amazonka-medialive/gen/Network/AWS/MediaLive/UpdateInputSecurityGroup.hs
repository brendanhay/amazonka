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
-- Module      : Network.AWS.MediaLive.UpdateInputSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an Input Security Group\'s Whilelists.
module Network.AWS.MediaLive.UpdateInputSecurityGroup
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update some combination of the Input Security Group name
-- and the IPv4 CIDRs the Input Security Group should allow.
--
-- /See:/ 'newUpdateInputSecurityGroup' smart constructor.
data UpdateInputSecurityGroup = UpdateInputSecurityGroup'
  { -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | List of IPv4 CIDR addresses to whitelist
    whitelistRules :: Core.Maybe [InputWhitelistRuleCidr],
    -- | The id of the Input Security Group to update.
    inputSecurityGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateInputSecurityGroup
newUpdateInputSecurityGroup pInputSecurityGroupId_ =
  UpdateInputSecurityGroup'
    { tags = Core.Nothing,
      whitelistRules = Core.Nothing,
      inputSecurityGroupId = pInputSecurityGroupId_
    }

-- | A collection of key-value pairs.
updateInputSecurityGroup_tags :: Lens.Lens' UpdateInputSecurityGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateInputSecurityGroup_tags = Lens.lens (\UpdateInputSecurityGroup' {tags} -> tags) (\s@UpdateInputSecurityGroup' {} a -> s {tags = a} :: UpdateInputSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | List of IPv4 CIDR addresses to whitelist
updateInputSecurityGroup_whitelistRules :: Lens.Lens' UpdateInputSecurityGroup (Core.Maybe [InputWhitelistRuleCidr])
updateInputSecurityGroup_whitelistRules = Lens.lens (\UpdateInputSecurityGroup' {whitelistRules} -> whitelistRules) (\s@UpdateInputSecurityGroup' {} a -> s {whitelistRules = a} :: UpdateInputSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | The id of the Input Security Group to update.
updateInputSecurityGroup_inputSecurityGroupId :: Lens.Lens' UpdateInputSecurityGroup Core.Text
updateInputSecurityGroup_inputSecurityGroupId = Lens.lens (\UpdateInputSecurityGroup' {inputSecurityGroupId} -> inputSecurityGroupId) (\s@UpdateInputSecurityGroup' {} a -> s {inputSecurityGroupId = a} :: UpdateInputSecurityGroup)

instance Core.AWSRequest UpdateInputSecurityGroup where
  type
    AWSResponse UpdateInputSecurityGroup =
      UpdateInputSecurityGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputSecurityGroupResponse'
            Core.<$> (x Core..?> "securityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateInputSecurityGroup

instance Core.NFData UpdateInputSecurityGroup

instance Core.ToHeaders UpdateInputSecurityGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateInputSecurityGroup where
  toJSON UpdateInputSecurityGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("whitelistRules" Core..=) Core.<$> whitelistRules
          ]
      )

instance Core.ToPath UpdateInputSecurityGroup where
  toPath UpdateInputSecurityGroup' {..} =
    Core.mconcat
      [ "/prod/inputSecurityGroups/",
        Core.toBS inputSecurityGroupId
      ]

instance Core.ToQuery UpdateInputSecurityGroup where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for UpdateInputSecurityGroupResponse
--
-- /See:/ 'newUpdateInputSecurityGroupResponse' smart constructor.
data UpdateInputSecurityGroupResponse = UpdateInputSecurityGroupResponse'
  { securityGroup :: Core.Maybe InputSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateInputSecurityGroupResponse
newUpdateInputSecurityGroupResponse pHttpStatus_ =
  UpdateInputSecurityGroupResponse'
    { securityGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateInputSecurityGroupResponse_securityGroup :: Lens.Lens' UpdateInputSecurityGroupResponse (Core.Maybe InputSecurityGroup)
updateInputSecurityGroupResponse_securityGroup = Lens.lens (\UpdateInputSecurityGroupResponse' {securityGroup} -> securityGroup) (\s@UpdateInputSecurityGroupResponse' {} a -> s {securityGroup = a} :: UpdateInputSecurityGroupResponse)

-- | The response's http status code.
updateInputSecurityGroupResponse_httpStatus :: Lens.Lens' UpdateInputSecurityGroupResponse Core.Int
updateInputSecurityGroupResponse_httpStatus = Lens.lens (\UpdateInputSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateInputSecurityGroupResponse' {} a -> s {httpStatus = a} :: UpdateInputSecurityGroupResponse)

instance Core.NFData UpdateInputSecurityGroupResponse

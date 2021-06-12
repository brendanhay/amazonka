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
-- Module      : Network.AWS.CloudFormation.UpdateTerminationProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates termination protection for the specified stack. If a user
-- attempts to delete a stack with termination protection enabled, the
-- operation fails and the stack remains unchanged. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /AWS CloudFormation User Guide/.
--
-- For
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and cannot be changed
-- directly on the nested stack.
module Network.AWS.CloudFormation.UpdateTerminationProtection
  ( -- * Creating a Request
    UpdateTerminationProtection (..),
    newUpdateTerminationProtection,

    -- * Request Lenses
    updateTerminationProtection_enableTerminationProtection,
    updateTerminationProtection_stackName,

    -- * Destructuring the Response
    UpdateTerminationProtectionResponse (..),
    newUpdateTerminationProtectionResponse,

    -- * Response Lenses
    updateTerminationProtectionResponse_stackId,
    updateTerminationProtectionResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTerminationProtection' smart constructor.
data UpdateTerminationProtection = UpdateTerminationProtection'
  { -- | Whether to enable termination protection on the specified stack.
    enableTerminationProtection :: Core.Bool,
    -- | The name or unique ID of the stack for which you want to set termination
    -- protection.
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTerminationProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableTerminationProtection', 'updateTerminationProtection_enableTerminationProtection' - Whether to enable termination protection on the specified stack.
--
-- 'stackName', 'updateTerminationProtection_stackName' - The name or unique ID of the stack for which you want to set termination
-- protection.
newUpdateTerminationProtection ::
  -- | 'enableTerminationProtection'
  Core.Bool ->
  -- | 'stackName'
  Core.Text ->
  UpdateTerminationProtection
newUpdateTerminationProtection
  pEnableTerminationProtection_
  pStackName_ =
    UpdateTerminationProtection'
      { enableTerminationProtection =
          pEnableTerminationProtection_,
        stackName = pStackName_
      }

-- | Whether to enable termination protection on the specified stack.
updateTerminationProtection_enableTerminationProtection :: Lens.Lens' UpdateTerminationProtection Core.Bool
updateTerminationProtection_enableTerminationProtection = Lens.lens (\UpdateTerminationProtection' {enableTerminationProtection} -> enableTerminationProtection) (\s@UpdateTerminationProtection' {} a -> s {enableTerminationProtection = a} :: UpdateTerminationProtection)

-- | The name or unique ID of the stack for which you want to set termination
-- protection.
updateTerminationProtection_stackName :: Lens.Lens' UpdateTerminationProtection Core.Text
updateTerminationProtection_stackName = Lens.lens (\UpdateTerminationProtection' {stackName} -> stackName) (\s@UpdateTerminationProtection' {} a -> s {stackName = a} :: UpdateTerminationProtection)

instance Core.AWSRequest UpdateTerminationProtection where
  type
    AWSResponse UpdateTerminationProtection =
      UpdateTerminationProtectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateTerminationProtectionResult"
      ( \s h x ->
          UpdateTerminationProtectionResponse'
            Core.<$> (x Core..@? "StackId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTerminationProtection

instance Core.NFData UpdateTerminationProtection

instance Core.ToHeaders UpdateTerminationProtection where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateTerminationProtection where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTerminationProtection where
  toQuery UpdateTerminationProtection' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateTerminationProtection" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "EnableTerminationProtection"
          Core.=: enableTerminationProtection,
        "StackName" Core.=: stackName
      ]

-- | /See:/ 'newUpdateTerminationProtectionResponse' smart constructor.
data UpdateTerminationProtectionResponse = UpdateTerminationProtectionResponse'
  { -- | The unique ID of the stack.
    stackId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTerminationProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'updateTerminationProtectionResponse_stackId' - The unique ID of the stack.
--
-- 'httpStatus', 'updateTerminationProtectionResponse_httpStatus' - The response's http status code.
newUpdateTerminationProtectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTerminationProtectionResponse
newUpdateTerminationProtectionResponse pHttpStatus_ =
  UpdateTerminationProtectionResponse'
    { stackId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the stack.
updateTerminationProtectionResponse_stackId :: Lens.Lens' UpdateTerminationProtectionResponse (Core.Maybe Core.Text)
updateTerminationProtectionResponse_stackId = Lens.lens (\UpdateTerminationProtectionResponse' {stackId} -> stackId) (\s@UpdateTerminationProtectionResponse' {} a -> s {stackId = a} :: UpdateTerminationProtectionResponse)

-- | The response's http status code.
updateTerminationProtectionResponse_httpStatus :: Lens.Lens' UpdateTerminationProtectionResponse Core.Int
updateTerminationProtectionResponse_httpStatus = Lens.lens (\UpdateTerminationProtectionResponse' {httpStatus} -> httpStatus) (\s@UpdateTerminationProtectionResponse' {} a -> s {httpStatus = a} :: UpdateTerminationProtectionResponse)

instance
  Core.NFData
    UpdateTerminationProtectionResponse

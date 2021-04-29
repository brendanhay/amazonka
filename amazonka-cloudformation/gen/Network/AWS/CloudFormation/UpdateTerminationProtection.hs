{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTerminationProtection' smart constructor.
data UpdateTerminationProtection = UpdateTerminationProtection'
  { -- | Whether to enable termination protection on the specified stack.
    enableTerminationProtection :: Prelude.Bool,
    -- | The name or unique ID of the stack for which you want to set termination
    -- protection.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Bool ->
  -- | 'stackName'
  Prelude.Text ->
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
updateTerminationProtection_enableTerminationProtection :: Lens.Lens' UpdateTerminationProtection Prelude.Bool
updateTerminationProtection_enableTerminationProtection = Lens.lens (\UpdateTerminationProtection' {enableTerminationProtection} -> enableTerminationProtection) (\s@UpdateTerminationProtection' {} a -> s {enableTerminationProtection = a} :: UpdateTerminationProtection)

-- | The name or unique ID of the stack for which you want to set termination
-- protection.
updateTerminationProtection_stackName :: Lens.Lens' UpdateTerminationProtection Prelude.Text
updateTerminationProtection_stackName = Lens.lens (\UpdateTerminationProtection' {stackName} -> stackName) (\s@UpdateTerminationProtection' {} a -> s {stackName = a} :: UpdateTerminationProtection)

instance
  Prelude.AWSRequest
    UpdateTerminationProtection
  where
  type
    Rs UpdateTerminationProtection =
      UpdateTerminationProtectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateTerminationProtectionResult"
      ( \s h x ->
          UpdateTerminationProtectionResponse'
            Prelude.<$> (x Prelude..@? "StackId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTerminationProtection

instance Prelude.NFData UpdateTerminationProtection

instance
  Prelude.ToHeaders
    UpdateTerminationProtection
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateTerminationProtection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTerminationProtection where
  toQuery UpdateTerminationProtection' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "UpdateTerminationProtection" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "EnableTerminationProtection"
          Prelude.=: enableTerminationProtection,
        "StackName" Prelude.=: stackName
      ]

-- | /See:/ 'newUpdateTerminationProtectionResponse' smart constructor.
data UpdateTerminationProtectionResponse = UpdateTerminationProtectionResponse'
  { -- | The unique ID of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateTerminationProtectionResponse
newUpdateTerminationProtectionResponse pHttpStatus_ =
  UpdateTerminationProtectionResponse'
    { stackId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the stack.
updateTerminationProtectionResponse_stackId :: Lens.Lens' UpdateTerminationProtectionResponse (Prelude.Maybe Prelude.Text)
updateTerminationProtectionResponse_stackId = Lens.lens (\UpdateTerminationProtectionResponse' {stackId} -> stackId) (\s@UpdateTerminationProtectionResponse' {} a -> s {stackId = a} :: UpdateTerminationProtectionResponse)

-- | The response's http status code.
updateTerminationProtectionResponse_httpStatus :: Lens.Lens' UpdateTerminationProtectionResponse Prelude.Int
updateTerminationProtectionResponse_httpStatus = Lens.lens (\UpdateTerminationProtectionResponse' {httpStatus} -> httpStatus) (\s@UpdateTerminationProtectionResponse' {} a -> s {httpStatus = a} :: UpdateTerminationProtectionResponse)

instance
  Prelude.NFData
    UpdateTerminationProtectionResponse

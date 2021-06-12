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
-- Module      : Network.AWS.CloudFormation.GetStackPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the stack policy for a specified stack. If a stack doesn\'t have
-- a policy, a null value is returned.
module Network.AWS.CloudFormation.GetStackPolicy
  ( -- * Creating a Request
    GetStackPolicy (..),
    newGetStackPolicy,

    -- * Request Lenses
    getStackPolicy_stackName,

    -- * Destructuring the Response
    GetStackPolicyResponse (..),
    newGetStackPolicyResponse,

    -- * Response Lenses
    getStackPolicyResponse_stackPolicyBody,
    getStackPolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetStackPolicy action.
--
-- /See:/ 'newGetStackPolicy' smart constructor.
data GetStackPolicy = GetStackPolicy'
  { -- | The name or unique stack ID that is associated with the stack whose
    -- policy you want to get.
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetStackPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'getStackPolicy_stackName' - The name or unique stack ID that is associated with the stack whose
-- policy you want to get.
newGetStackPolicy ::
  -- | 'stackName'
  Core.Text ->
  GetStackPolicy
newGetStackPolicy pStackName_ =
  GetStackPolicy' {stackName = pStackName_}

-- | The name or unique stack ID that is associated with the stack whose
-- policy you want to get.
getStackPolicy_stackName :: Lens.Lens' GetStackPolicy Core.Text
getStackPolicy_stackName = Lens.lens (\GetStackPolicy' {stackName} -> stackName) (\s@GetStackPolicy' {} a -> s {stackName = a} :: GetStackPolicy)

instance Core.AWSRequest GetStackPolicy where
  type
    AWSResponse GetStackPolicy =
      GetStackPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetStackPolicyResult"
      ( \s h x ->
          GetStackPolicyResponse'
            Core.<$> (x Core..@? "StackPolicyBody")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetStackPolicy

instance Core.NFData GetStackPolicy

instance Core.ToHeaders GetStackPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetStackPolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetStackPolicy where
  toQuery GetStackPolicy' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetStackPolicy" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "StackName" Core.=: stackName
      ]

-- | The output for the GetStackPolicy action.
--
-- /See:/ 'newGetStackPolicyResponse' smart constructor.
data GetStackPolicyResponse = GetStackPolicyResponse'
  { -- | Structure containing the stack policy body. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
    -- in the AWS CloudFormation User Guide.)
    stackPolicyBody :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetStackPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackPolicyBody', 'getStackPolicyResponse_stackPolicyBody' - Structure containing the stack policy body. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide.)
--
-- 'httpStatus', 'getStackPolicyResponse_httpStatus' - The response's http status code.
newGetStackPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetStackPolicyResponse
newGetStackPolicyResponse pHttpStatus_ =
  GetStackPolicyResponse'
    { stackPolicyBody =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Structure containing the stack policy body. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide.)
getStackPolicyResponse_stackPolicyBody :: Lens.Lens' GetStackPolicyResponse (Core.Maybe Core.Text)
getStackPolicyResponse_stackPolicyBody = Lens.lens (\GetStackPolicyResponse' {stackPolicyBody} -> stackPolicyBody) (\s@GetStackPolicyResponse' {} a -> s {stackPolicyBody = a} :: GetStackPolicyResponse)

-- | The response's http status code.
getStackPolicyResponse_httpStatus :: Lens.Lens' GetStackPolicyResponse Core.Int
getStackPolicyResponse_httpStatus = Lens.lens (\GetStackPolicyResponse' {httpStatus} -> httpStatus) (\s@GetStackPolicyResponse' {} a -> s {httpStatus = a} :: GetStackPolicyResponse)

instance Core.NFData GetStackPolicyResponse

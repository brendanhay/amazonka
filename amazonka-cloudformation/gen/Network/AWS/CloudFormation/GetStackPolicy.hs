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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetStackPolicy action.
--
-- /See:/ 'newGetStackPolicy' smart constructor.
data GetStackPolicy = GetStackPolicy'
  { -- | The name or unique stack ID that is associated with the stack whose
    -- policy you want to get.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetStackPolicy
newGetStackPolicy pStackName_ =
  GetStackPolicy' {stackName = pStackName_}

-- | The name or unique stack ID that is associated with the stack whose
-- policy you want to get.
getStackPolicy_stackName :: Lens.Lens' GetStackPolicy Prelude.Text
getStackPolicy_stackName = Lens.lens (\GetStackPolicy' {stackName} -> stackName) (\s@GetStackPolicy' {} a -> s {stackName = a} :: GetStackPolicy)

instance Prelude.AWSRequest GetStackPolicy where
  type Rs GetStackPolicy = GetStackPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetStackPolicyResult"
      ( \s h x ->
          GetStackPolicyResponse'
            Prelude.<$> (x Prelude..@? "StackPolicyBody")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStackPolicy

instance Prelude.NFData GetStackPolicy

instance Prelude.ToHeaders GetStackPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetStackPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetStackPolicy where
  toQuery GetStackPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetStackPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "StackName" Prelude.=: stackName
      ]

-- | The output for the GetStackPolicy action.
--
-- /See:/ 'newGetStackPolicyResponse' smart constructor.
data GetStackPolicyResponse = GetStackPolicyResponse'
  { -- | Structure containing the stack policy body. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
    -- in the AWS CloudFormation User Guide.)
    stackPolicyBody :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetStackPolicyResponse
newGetStackPolicyResponse pHttpStatus_ =
  GetStackPolicyResponse'
    { stackPolicyBody =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Structure containing the stack policy body. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide.)
getStackPolicyResponse_stackPolicyBody :: Lens.Lens' GetStackPolicyResponse (Prelude.Maybe Prelude.Text)
getStackPolicyResponse_stackPolicyBody = Lens.lens (\GetStackPolicyResponse' {stackPolicyBody} -> stackPolicyBody) (\s@GetStackPolicyResponse' {} a -> s {stackPolicyBody = a} :: GetStackPolicyResponse)

-- | The response's http status code.
getStackPolicyResponse_httpStatus :: Lens.Lens' GetStackPolicyResponse Prelude.Int
getStackPolicyResponse_httpStatus = Lens.lens (\GetStackPolicyResponse' {httpStatus} -> httpStatus) (\s@GetStackPolicyResponse' {} a -> s {httpStatus = a} :: GetStackPolicyResponse)

instance Prelude.NFData GetStackPolicyResponse

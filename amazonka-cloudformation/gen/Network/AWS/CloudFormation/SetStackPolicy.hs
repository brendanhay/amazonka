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
-- Module      : Network.AWS.CloudFormation.SetStackPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a stack policy for a specified stack.
module Network.AWS.CloudFormation.SetStackPolicy
  ( -- * Creating a Request
    SetStackPolicy (..),
    newSetStackPolicy,

    -- * Request Lenses
    setStackPolicy_stackPolicyBody,
    setStackPolicy_stackPolicyURL,
    setStackPolicy_stackName,

    -- * Destructuring the Response
    SetStackPolicyResponse (..),
    newSetStackPolicyResponse,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the SetStackPolicy action.
--
-- /See:/ 'newSetStackPolicy' smart constructor.
data SetStackPolicy = SetStackPolicy'
  { -- | Structure containing the stack policy body. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
    -- in the AWS CloudFormation User Guide. You can specify either the
    -- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyBody :: Core.Maybe Core.Text,
    -- | Location of a file containing the stack policy. The URL must point to a
    -- policy (maximum size: 16 KB) located in an S3 bucket in the same Region
    -- as the stack. You can specify either the @StackPolicyBody@ or the
    -- @StackPolicyURL@ parameter, but not both.
    stackPolicyURL :: Core.Maybe Core.Text,
    -- | The name or unique stack ID that you want to associate a policy with.
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetStackPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackPolicyBody', 'setStackPolicy_stackPolicyBody' - Structure containing the stack policy body. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- 'stackPolicyURL', 'setStackPolicy_stackPolicyURL' - Location of a file containing the stack policy. The URL must point to a
-- policy (maximum size: 16 KB) located in an S3 bucket in the same Region
-- as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
--
-- 'stackName', 'setStackPolicy_stackName' - The name or unique stack ID that you want to associate a policy with.
newSetStackPolicy ::
  -- | 'stackName'
  Core.Text ->
  SetStackPolicy
newSetStackPolicy pStackName_ =
  SetStackPolicy'
    { stackPolicyBody = Core.Nothing,
      stackPolicyURL = Core.Nothing,
      stackName = pStackName_
    }

-- | Structure containing the stack policy body. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
setStackPolicy_stackPolicyBody :: Lens.Lens' SetStackPolicy (Core.Maybe Core.Text)
setStackPolicy_stackPolicyBody = Lens.lens (\SetStackPolicy' {stackPolicyBody} -> stackPolicyBody) (\s@SetStackPolicy' {} a -> s {stackPolicyBody = a} :: SetStackPolicy)

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (maximum size: 16 KB) located in an S3 bucket in the same Region
-- as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
setStackPolicy_stackPolicyURL :: Lens.Lens' SetStackPolicy (Core.Maybe Core.Text)
setStackPolicy_stackPolicyURL = Lens.lens (\SetStackPolicy' {stackPolicyURL} -> stackPolicyURL) (\s@SetStackPolicy' {} a -> s {stackPolicyURL = a} :: SetStackPolicy)

-- | The name or unique stack ID that you want to associate a policy with.
setStackPolicy_stackName :: Lens.Lens' SetStackPolicy Core.Text
setStackPolicy_stackName = Lens.lens (\SetStackPolicy' {stackName} -> stackName) (\s@SetStackPolicy' {} a -> s {stackName = a} :: SetStackPolicy)

instance Core.AWSRequest SetStackPolicy where
  type
    AWSResponse SetStackPolicy =
      SetStackPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SetStackPolicyResponse'

instance Core.Hashable SetStackPolicy

instance Core.NFData SetStackPolicy

instance Core.ToHeaders SetStackPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetStackPolicy where
  toPath = Core.const "/"

instance Core.ToQuery SetStackPolicy where
  toQuery SetStackPolicy' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SetStackPolicy" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "StackPolicyBody" Core.=: stackPolicyBody,
        "StackPolicyURL" Core.=: stackPolicyURL,
        "StackName" Core.=: stackName
      ]

-- | /See:/ 'newSetStackPolicyResponse' smart constructor.
data SetStackPolicyResponse = SetStackPolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetStackPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetStackPolicyResponse ::
  SetStackPolicyResponse
newSetStackPolicyResponse = SetStackPolicyResponse'

instance Core.NFData SetStackPolicyResponse

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    stackPolicyBody :: Prelude.Maybe Prelude.Text,
    -- | Location of a file containing the stack policy. The URL must point to a
    -- policy (maximum size: 16 KB) located in an S3 bucket in the same Region
    -- as the stack. You can specify either the @StackPolicyBody@ or the
    -- @StackPolicyURL@ parameter, but not both.
    stackPolicyURL :: Prelude.Maybe Prelude.Text,
    -- | The name or unique stack ID that you want to associate a policy with.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  SetStackPolicy
newSetStackPolicy pStackName_ =
  SetStackPolicy'
    { stackPolicyBody = Prelude.Nothing,
      stackPolicyURL = Prelude.Nothing,
      stackName = pStackName_
    }

-- | Structure containing the stack policy body. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
setStackPolicy_stackPolicyBody :: Lens.Lens' SetStackPolicy (Prelude.Maybe Prelude.Text)
setStackPolicy_stackPolicyBody = Lens.lens (\SetStackPolicy' {stackPolicyBody} -> stackPolicyBody) (\s@SetStackPolicy' {} a -> s {stackPolicyBody = a} :: SetStackPolicy)

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (maximum size: 16 KB) located in an S3 bucket in the same Region
-- as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
setStackPolicy_stackPolicyURL :: Lens.Lens' SetStackPolicy (Prelude.Maybe Prelude.Text)
setStackPolicy_stackPolicyURL = Lens.lens (\SetStackPolicy' {stackPolicyURL} -> stackPolicyURL) (\s@SetStackPolicy' {} a -> s {stackPolicyURL = a} :: SetStackPolicy)

-- | The name or unique stack ID that you want to associate a policy with.
setStackPolicy_stackName :: Lens.Lens' SetStackPolicy Prelude.Text
setStackPolicy_stackName = Lens.lens (\SetStackPolicy' {stackName} -> stackName) (\s@SetStackPolicy' {} a -> s {stackName = a} :: SetStackPolicy)

instance Prelude.AWSRequest SetStackPolicy where
  type Rs SetStackPolicy = SetStackPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SetStackPolicyResponse'

instance Prelude.Hashable SetStackPolicy

instance Prelude.NFData SetStackPolicy

instance Prelude.ToHeaders SetStackPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SetStackPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetStackPolicy where
  toQuery SetStackPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SetStackPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "StackPolicyBody" Prelude.=: stackPolicyBody,
        "StackPolicyURL" Prelude.=: stackPolicyURL,
        "StackName" Prelude.=: stackName
      ]

-- | /See:/ 'newSetStackPolicyResponse' smart constructor.
data SetStackPolicyResponse = SetStackPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetStackPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetStackPolicyResponse ::
  SetStackPolicyResponse
newSetStackPolicyResponse = SetStackPolicyResponse'

instance Prelude.NFData SetStackPolicyResponse

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SetStackPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a stack policy for a specified stack.
module Network.AWS.CloudFormation.SetStackPolicy
  ( -- * Creating a request
    SetStackPolicy (..),
    mkSetStackPolicy,

    -- ** Request lenses
    sspStackPolicyBody,
    sspStackPolicyURL,
    sspStackName,

    -- * Destructuring the response
    SetStackPolicyResponse (..),
    mkSetStackPolicyResponse,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'SetStackPolicy' action.
--
-- /See:/ 'mkSetStackPolicy' smart constructor.
data SetStackPolicy = SetStackPolicy'
  { -- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyBody :: Lude.Maybe Lude.Text,
    -- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyURL :: Lude.Maybe Lude.Text,
    -- | The name or unique stack ID that you want to associate a policy with.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetStackPolicy' with the minimum fields required to make a request.
--
-- * 'stackPolicyBody' - Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
-- * 'stackPolicyURL' - Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
-- * 'stackName' - The name or unique stack ID that you want to associate a policy with.
mkSetStackPolicy ::
  -- | 'stackName'
  Lude.Text ->
  SetStackPolicy
mkSetStackPolicy pStackName_ =
  SetStackPolicy'
    { stackPolicyBody = Lude.Nothing,
      stackPolicyURL = Lude.Nothing,
      stackName = pStackName_
    }

-- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackPolicyBody :: Lens.Lens' SetStackPolicy (Lude.Maybe Lude.Text)
sspStackPolicyBody = Lens.lens (stackPolicyBody :: SetStackPolicy -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyBody = a} :: SetStackPolicy)
{-# DEPRECATED sspStackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead." #-}

-- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackPolicyURL :: Lens.Lens' SetStackPolicy (Lude.Maybe Lude.Text)
sspStackPolicyURL = Lens.lens (stackPolicyURL :: SetStackPolicy -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyURL = a} :: SetStackPolicy)
{-# DEPRECATED sspStackPolicyURL "Use generic-lens or generic-optics with 'stackPolicyURL' instead." #-}

-- | The name or unique stack ID that you want to associate a policy with.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspStackName :: Lens.Lens' SetStackPolicy Lude.Text
sspStackName = Lens.lens (stackName :: SetStackPolicy -> Lude.Text) (\s a -> s {stackName = a} :: SetStackPolicy)
{-# DEPRECATED sspStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest SetStackPolicy where
  type Rs SetStackPolicy = SetStackPolicyResponse
  request = Req.postQuery cloudFormationService
  response = Res.receiveNull SetStackPolicyResponse'

instance Lude.ToHeaders SetStackPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetStackPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery SetStackPolicy where
  toQuery SetStackPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetStackPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "StackPolicyBody" Lude.=: stackPolicyBody,
        "StackPolicyURL" Lude.=: stackPolicyURL,
        "StackName" Lude.=: stackName
      ]

-- | /See:/ 'mkSetStackPolicyResponse' smart constructor.
data SetStackPolicyResponse = SetStackPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetStackPolicyResponse' with the minimum fields required to make a request.
mkSetStackPolicyResponse ::
  SetStackPolicyResponse
mkSetStackPolicyResponse = SetStackPolicyResponse'

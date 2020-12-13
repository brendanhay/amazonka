{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.GetStackPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the stack policy for a specified stack. If a stack doesn't have a policy, a null value is returned.
module Network.AWS.CloudFormation.GetStackPolicy
  ( -- * Creating a request
    GetStackPolicy (..),
    mkGetStackPolicy,

    -- ** Request lenses
    gspStackName,

    -- * Destructuring the response
    GetStackPolicyResponse (..),
    mkGetStackPolicyResponse,

    -- ** Response lenses
    gsprsStackPolicyBody,
    gsprsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'GetStackPolicy' action.
--
-- /See:/ 'mkGetStackPolicy' smart constructor.
newtype GetStackPolicy = GetStackPolicy'
  { -- | The name or unique stack ID that is associated with the stack whose policy you want to get.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStackPolicy' with the minimum fields required to make a request.
--
-- * 'stackName' - The name or unique stack ID that is associated with the stack whose policy you want to get.
mkGetStackPolicy ::
  -- | 'stackName'
  Lude.Text ->
  GetStackPolicy
mkGetStackPolicy pStackName_ =
  GetStackPolicy' {stackName = pStackName_}

-- | The name or unique stack ID that is associated with the stack whose policy you want to get.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspStackName :: Lens.Lens' GetStackPolicy Lude.Text
gspStackName = Lens.lens (stackName :: GetStackPolicy -> Lude.Text) (\s a -> s {stackName = a} :: GetStackPolicy)
{-# DEPRECATED gspStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest GetStackPolicy where
  type Rs GetStackPolicy = GetStackPolicyResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "GetStackPolicyResult"
      ( \s h x ->
          GetStackPolicyResponse'
            Lude.<$> (x Lude..@? "StackPolicyBody")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStackPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetStackPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetStackPolicy where
  toQuery GetStackPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetStackPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "StackName" Lude.=: stackName
      ]

-- | The output for the 'GetStackPolicy' action.
--
-- /See:/ 'mkGetStackPolicyResponse' smart constructor.
data GetStackPolicyResponse = GetStackPolicyResponse'
  { -- | Structure containing the stack policy body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide.)
    stackPolicyBody :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStackPolicyResponse' with the minimum fields required to make a request.
--
-- * 'stackPolicyBody' - Structure containing the stack policy body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide.)
-- * 'responseStatus' - The response status code.
mkGetStackPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStackPolicyResponse
mkGetStackPolicyResponse pResponseStatus_ =
  GetStackPolicyResponse'
    { stackPolicyBody = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Structure containing the stack policy body. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide.)
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprsStackPolicyBody :: Lens.Lens' GetStackPolicyResponse (Lude.Maybe Lude.Text)
gsprsStackPolicyBody = Lens.lens (stackPolicyBody :: GetStackPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyBody = a} :: GetStackPolicyResponse)
{-# DEPRECATED gsprsStackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprsResponseStatus :: Lens.Lens' GetStackPolicyResponse Lude.Int
gsprsResponseStatus = Lens.lens (responseStatus :: GetStackPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStackPolicyResponse)
{-# DEPRECATED gsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

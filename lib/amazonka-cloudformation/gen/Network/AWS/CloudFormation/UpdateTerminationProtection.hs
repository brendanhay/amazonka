{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.UpdateTerminationProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates termination protection for the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
--
-- For <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack.
module Network.AWS.CloudFormation.UpdateTerminationProtection
  ( -- * Creating a request
    UpdateTerminationProtection (..),
    mkUpdateTerminationProtection,

    -- ** Request lenses
    utpEnableTerminationProtection,
    utpStackName,

    -- * Destructuring the response
    UpdateTerminationProtectionResponse (..),
    mkUpdateTerminationProtectionResponse,

    -- ** Response lenses
    utprsStackId,
    utprsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTerminationProtection' smart constructor.
data UpdateTerminationProtection = UpdateTerminationProtection'
  { -- | Whether to enable termination protection on the specified stack.
    enableTerminationProtection :: Lude.Bool,
    -- | The name or unique ID of the stack for which you want to set termination protection.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTerminationProtection' with the minimum fields required to make a request.
--
-- * 'enableTerminationProtection' - Whether to enable termination protection on the specified stack.
-- * 'stackName' - The name or unique ID of the stack for which you want to set termination protection.
mkUpdateTerminationProtection ::
  -- | 'enableTerminationProtection'
  Lude.Bool ->
  -- | 'stackName'
  Lude.Text ->
  UpdateTerminationProtection
mkUpdateTerminationProtection
  pEnableTerminationProtection_
  pStackName_ =
    UpdateTerminationProtection'
      { enableTerminationProtection =
          pEnableTerminationProtection_,
        stackName = pStackName_
      }

-- | Whether to enable termination protection on the specified stack.
--
-- /Note:/ Consider using 'enableTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpEnableTerminationProtection :: Lens.Lens' UpdateTerminationProtection Lude.Bool
utpEnableTerminationProtection = Lens.lens (enableTerminationProtection :: UpdateTerminationProtection -> Lude.Bool) (\s a -> s {enableTerminationProtection = a} :: UpdateTerminationProtection)
{-# DEPRECATED utpEnableTerminationProtection "Use generic-lens or generic-optics with 'enableTerminationProtection' instead." #-}

-- | The name or unique ID of the stack for which you want to set termination protection.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utpStackName :: Lens.Lens' UpdateTerminationProtection Lude.Text
utpStackName = Lens.lens (stackName :: UpdateTerminationProtection -> Lude.Text) (\s a -> s {stackName = a} :: UpdateTerminationProtection)
{-# DEPRECATED utpStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest UpdateTerminationProtection where
  type
    Rs UpdateTerminationProtection =
      UpdateTerminationProtectionResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "UpdateTerminationProtectionResult"
      ( \s h x ->
          UpdateTerminationProtectionResponse'
            Lude.<$> (x Lude..@? "StackId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTerminationProtection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateTerminationProtection where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTerminationProtection where
  toQuery UpdateTerminationProtection' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateTerminationProtection" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "EnableTerminationProtection" Lude.=: enableTerminationProtection,
        "StackName" Lude.=: stackName
      ]

-- | /See:/ 'mkUpdateTerminationProtectionResponse' smart constructor.
data UpdateTerminationProtectionResponse = UpdateTerminationProtectionResponse'
  { -- | The unique ID of the stack.
    stackId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTerminationProtectionResponse' with the minimum fields required to make a request.
--
-- * 'stackId' - The unique ID of the stack.
-- * 'responseStatus' - The response status code.
mkUpdateTerminationProtectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTerminationProtectionResponse
mkUpdateTerminationProtectionResponse pResponseStatus_ =
  UpdateTerminationProtectionResponse'
    { stackId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utprsStackId :: Lens.Lens' UpdateTerminationProtectionResponse (Lude.Maybe Lude.Text)
utprsStackId = Lens.lens (stackId :: UpdateTerminationProtectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: UpdateTerminationProtectionResponse)
{-# DEPRECATED utprsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utprsResponseStatus :: Lens.Lens' UpdateTerminationProtectionResponse Lude.Int
utprsResponseStatus = Lens.lens (responseStatus :: UpdateTerminationProtectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTerminationProtectionResponse)
{-# DEPRECATED utprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.UpdateParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.
module Network.AWS.DAX.UpdateParameterGroup
  ( -- * Creating a request
    UpdateParameterGroup (..),
    mkUpdateParameterGroup,

    -- ** Request lenses
    upgParameterNameValues,
    upgParameterGroupName,

    -- * Destructuring the response
    UpdateParameterGroupResponse (..),
    mkUpdateParameterGroupResponse,

    -- ** Response lenses
    upgrsParameterGroup,
    upgrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateParameterGroup' smart constructor.
data UpdateParameterGroup = UpdateParameterGroup'
  { -- | An array of name-value pairs for the parameters in the group. Each element in the array represents a single parameter.
    parameterNameValues :: [ParameterNameValue],
    -- | The name of the parameter group.
    parameterGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateParameterGroup' with the minimum fields required to make a request.
--
-- * 'parameterNameValues' - An array of name-value pairs for the parameters in the group. Each element in the array represents a single parameter.
-- * 'parameterGroupName' - The name of the parameter group.
mkUpdateParameterGroup ::
  -- | 'parameterGroupName'
  Lude.Text ->
  UpdateParameterGroup
mkUpdateParameterGroup pParameterGroupName_ =
  UpdateParameterGroup'
    { parameterNameValues = Lude.mempty,
      parameterGroupName = pParameterGroupName_
    }

-- | An array of name-value pairs for the parameters in the group. Each element in the array represents a single parameter.
--
-- /Note:/ Consider using 'parameterNameValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgParameterNameValues :: Lens.Lens' UpdateParameterGroup [ParameterNameValue]
upgParameterNameValues = Lens.lens (parameterNameValues :: UpdateParameterGroup -> [ParameterNameValue]) (\s a -> s {parameterNameValues = a} :: UpdateParameterGroup)
{-# DEPRECATED upgParameterNameValues "Use generic-lens or generic-optics with 'parameterNameValues' instead." #-}

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgParameterGroupName :: Lens.Lens' UpdateParameterGroup Lude.Text
upgParameterGroupName = Lens.lens (parameterGroupName :: UpdateParameterGroup -> Lude.Text) (\s a -> s {parameterGroupName = a} :: UpdateParameterGroup)
{-# DEPRECATED upgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.AWSRequest UpdateParameterGroup where
  type Rs UpdateParameterGroup = UpdateParameterGroupResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateParameterGroupResponse'
            Lude.<$> (x Lude..?> "ParameterGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateParameterGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.UpdateParameterGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateParameterGroup where
  toJSON UpdateParameterGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ParameterNameValues" Lude..= parameterNameValues),
            Lude.Just ("ParameterGroupName" Lude..= parameterGroupName)
          ]
      )

instance Lude.ToPath UpdateParameterGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateParameterGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateParameterGroupResponse' smart constructor.
data UpdateParameterGroupResponse = UpdateParameterGroupResponse'
  { -- | The parameter group that has been modified.
    parameterGroup :: Lude.Maybe ParameterGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateParameterGroupResponse' with the minimum fields required to make a request.
--
-- * 'parameterGroup' - The parameter group that has been modified.
-- * 'responseStatus' - The response status code.
mkUpdateParameterGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateParameterGroupResponse
mkUpdateParameterGroupResponse pResponseStatus_ =
  UpdateParameterGroupResponse'
    { parameterGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The parameter group that has been modified.
--
-- /Note:/ Consider using 'parameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgrsParameterGroup :: Lens.Lens' UpdateParameterGroupResponse (Lude.Maybe ParameterGroup)
upgrsParameterGroup = Lens.lens (parameterGroup :: UpdateParameterGroupResponse -> Lude.Maybe ParameterGroup) (\s a -> s {parameterGroup = a} :: UpdateParameterGroupResponse)
{-# DEPRECATED upgrsParameterGroup "Use generic-lens or generic-optics with 'parameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgrsResponseStatus :: Lens.Lens' UpdateParameterGroupResponse Lude.Int
upgrsResponseStatus = Lens.lens (responseStatus :: UpdateParameterGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateParameterGroupResponse)
{-# DEPRECATED upgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.UpdateWorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the workgroup with the specified name. The workgroup's name cannot be changed.
module Network.AWS.Athena.UpdateWorkGroup
  ( -- * Creating a request
    UpdateWorkGroup (..),
    mkUpdateWorkGroup,

    -- ** Request lenses
    uwgState,
    uwgConfigurationUpdates,
    uwgDescription,
    uwgWorkGroup,

    -- * Destructuring the response
    UpdateWorkGroupResponse (..),
    mkUpdateWorkGroupResponse,

    -- ** Response lenses
    uwgrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateWorkGroup' smart constructor.
data UpdateWorkGroup = UpdateWorkGroup'
  { state ::
      Lude.Maybe WorkGroupState,
    configurationUpdates ::
      Lude.Maybe WorkGroupConfigurationUpdates,
    description :: Lude.Maybe Lude.Text,
    workGroup :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkGroup' with the minimum fields required to make a request.
--
-- * 'configurationUpdates' - The workgroup configuration that will be updated for the given workgroup.
-- * 'description' - The workgroup description.
-- * 'state' - The workgroup state that will be updated for the given workgroup.
-- * 'workGroup' - The specified workgroup that will be updated.
mkUpdateWorkGroup ::
  -- | 'workGroup'
  Lude.Text ->
  UpdateWorkGroup
mkUpdateWorkGroup pWorkGroup_ =
  UpdateWorkGroup'
    { state = Lude.Nothing,
      configurationUpdates = Lude.Nothing,
      description = Lude.Nothing,
      workGroup = pWorkGroup_
    }

-- | The workgroup state that will be updated for the given workgroup.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgState :: Lens.Lens' UpdateWorkGroup (Lude.Maybe WorkGroupState)
uwgState = Lens.lens (state :: UpdateWorkGroup -> Lude.Maybe WorkGroupState) (\s a -> s {state = a} :: UpdateWorkGroup)
{-# DEPRECATED uwgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The workgroup configuration that will be updated for the given workgroup.
--
-- /Note:/ Consider using 'configurationUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgConfigurationUpdates :: Lens.Lens' UpdateWorkGroup (Lude.Maybe WorkGroupConfigurationUpdates)
uwgConfigurationUpdates = Lens.lens (configurationUpdates :: UpdateWorkGroup -> Lude.Maybe WorkGroupConfigurationUpdates) (\s a -> s {configurationUpdates = a} :: UpdateWorkGroup)
{-# DEPRECATED uwgConfigurationUpdates "Use generic-lens or generic-optics with 'configurationUpdates' instead." #-}

-- | The workgroup description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgDescription :: Lens.Lens' UpdateWorkGroup (Lude.Maybe Lude.Text)
uwgDescription = Lens.lens (description :: UpdateWorkGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateWorkGroup)
{-# DEPRECATED uwgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The specified workgroup that will be updated.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgWorkGroup :: Lens.Lens' UpdateWorkGroup Lude.Text
uwgWorkGroup = Lens.lens (workGroup :: UpdateWorkGroup -> Lude.Text) (\s a -> s {workGroup = a} :: UpdateWorkGroup)
{-# DEPRECATED uwgWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Lude.AWSRequest UpdateWorkGroup where
  type Rs UpdateWorkGroup = UpdateWorkGroupResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateWorkGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateWorkGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.UpdateWorkGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateWorkGroup where
  toJSON UpdateWorkGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("State" Lude..=) Lude.<$> state,
            ("ConfigurationUpdates" Lude..=) Lude.<$> configurationUpdates,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("WorkGroup" Lude..= workGroup)
          ]
      )

instance Lude.ToPath UpdateWorkGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateWorkGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateWorkGroupResponse' smart constructor.
newtype UpdateWorkGroupResponse = UpdateWorkGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateWorkGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateWorkGroupResponse
mkUpdateWorkGroupResponse pResponseStatus_ =
  UpdateWorkGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwgrsResponseStatus :: Lens.Lens' UpdateWorkGroupResponse Lude.Int
uwgrsResponseStatus = Lens.lens (responseStatus :: UpdateWorkGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateWorkGroupResponse)
{-# DEPRECATED uwgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

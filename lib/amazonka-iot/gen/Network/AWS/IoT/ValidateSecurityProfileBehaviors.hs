{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ValidateSecurityProfileBehaviors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates a Device Defender security profile behaviors specification.
module Network.AWS.IoT.ValidateSecurityProfileBehaviors
  ( -- * Creating a request
    ValidateSecurityProfileBehaviors (..),
    mkValidateSecurityProfileBehaviors,

    -- ** Request lenses
    vspbBehaviors,

    -- * Destructuring the response
    ValidateSecurityProfileBehaviorsResponse (..),
    mkValidateSecurityProfileBehaviorsResponse,

    -- ** Response lenses
    vspbrsValidationErrors,
    vspbrsValid,
    vspbrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkValidateSecurityProfileBehaviors' smart constructor.
newtype ValidateSecurityProfileBehaviors = ValidateSecurityProfileBehaviors'
  { -- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
    behaviors :: [Behavior]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidateSecurityProfileBehaviors' with the minimum fields required to make a request.
--
-- * 'behaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
mkValidateSecurityProfileBehaviors ::
  ValidateSecurityProfileBehaviors
mkValidateSecurityProfileBehaviors =
  ValidateSecurityProfileBehaviors' {behaviors = Lude.mempty}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspbBehaviors :: Lens.Lens' ValidateSecurityProfileBehaviors [Behavior]
vspbBehaviors = Lens.lens (behaviors :: ValidateSecurityProfileBehaviors -> [Behavior]) (\s a -> s {behaviors = a} :: ValidateSecurityProfileBehaviors)
{-# DEPRECATED vspbBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

instance Lude.AWSRequest ValidateSecurityProfileBehaviors where
  type
    Rs ValidateSecurityProfileBehaviors =
      ValidateSecurityProfileBehaviorsResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ValidateSecurityProfileBehaviorsResponse'
            Lude.<$> (x Lude..?> "validationErrors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "valid")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ValidateSecurityProfileBehaviors where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ValidateSecurityProfileBehaviors where
  toJSON ValidateSecurityProfileBehaviors' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("behaviors" Lude..= behaviors)])

instance Lude.ToPath ValidateSecurityProfileBehaviors where
  toPath = Lude.const "/security-profile-behaviors/validate"

instance Lude.ToQuery ValidateSecurityProfileBehaviors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkValidateSecurityProfileBehaviorsResponse' smart constructor.
data ValidateSecurityProfileBehaviorsResponse = ValidateSecurityProfileBehaviorsResponse'
  { -- | The list of any errors found in the behaviors.
    validationErrors :: Lude.Maybe [ValidationError],
    -- | True if the behaviors were valid.
    valid :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidateSecurityProfileBehaviorsResponse' with the minimum fields required to make a request.
--
-- * 'validationErrors' - The list of any errors found in the behaviors.
-- * 'valid' - True if the behaviors were valid.
-- * 'responseStatus' - The response status code.
mkValidateSecurityProfileBehaviorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ValidateSecurityProfileBehaviorsResponse
mkValidateSecurityProfileBehaviorsResponse pResponseStatus_ =
  ValidateSecurityProfileBehaviorsResponse'
    { validationErrors =
        Lude.Nothing,
      valid = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of any errors found in the behaviors.
--
-- /Note:/ Consider using 'validationErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspbrsValidationErrors :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Lude.Maybe [ValidationError])
vspbrsValidationErrors = Lens.lens (validationErrors :: ValidateSecurityProfileBehaviorsResponse -> Lude.Maybe [ValidationError]) (\s a -> s {validationErrors = a} :: ValidateSecurityProfileBehaviorsResponse)
{-# DEPRECATED vspbrsValidationErrors "Use generic-lens or generic-optics with 'validationErrors' instead." #-}

-- | True if the behaviors were valid.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspbrsValid :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Lude.Maybe Lude.Bool)
vspbrsValid = Lens.lens (valid :: ValidateSecurityProfileBehaviorsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {valid = a} :: ValidateSecurityProfileBehaviorsResponse)
{-# DEPRECATED vspbrsValid "Use generic-lens or generic-optics with 'valid' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspbrsResponseStatus :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse Lude.Int
vspbrsResponseStatus = Lens.lens (responseStatus :: ValidateSecurityProfileBehaviorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ValidateSecurityProfileBehaviorsResponse)
{-# DEPRECATED vspbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

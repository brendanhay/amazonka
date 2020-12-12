{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationAction
  ( MitigationAction (..),

    -- * Smart constructor
    mkMitigationAction,

    -- * Lenses
    maActionParams,
    maName,
    maId,
    maRoleARN,
  )
where

import Network.AWS.IoT.Types.MitigationActionParams
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes which changes should be applied as part of a mitigation action.
--
-- /See:/ 'mkMitigationAction' smart constructor.
data MitigationAction = MitigationAction'
  { actionParams ::
      Lude.Maybe MitigationActionParams,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MitigationAction' with the minimum fields required to make a request.
--
-- * 'actionParams' - The set of parameters for this mitigation action. The parameters vary, depending on the kind of action you apply.
-- * 'id' - A unique identifier for the mitigation action.
-- * 'name' - A user-friendly name for the mitigation action.
-- * 'roleARN' - The IAM role ARN used to apply this mitigation action.
mkMitigationAction ::
  MitigationAction
mkMitigationAction =
  MitigationAction'
    { actionParams = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The set of parameters for this mitigation action. The parameters vary, depending on the kind of action you apply.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionParams :: Lens.Lens' MitigationAction (Lude.Maybe MitigationActionParams)
maActionParams = Lens.lens (actionParams :: MitigationAction -> Lude.Maybe MitigationActionParams) (\s a -> s {actionParams = a} :: MitigationAction)
{-# DEPRECATED maActionParams "Use generic-lens or generic-optics with 'actionParams' instead." #-}

-- | A user-friendly name for the mitigation action.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maName :: Lens.Lens' MitigationAction (Lude.Maybe Lude.Text)
maName = Lens.lens (name :: MitigationAction -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MitigationAction)
{-# DEPRECATED maName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for the mitigation action.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maId :: Lens.Lens' MitigationAction (Lude.Maybe Lude.Text)
maId = Lens.lens (id :: MitigationAction -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: MitigationAction)
{-# DEPRECATED maId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The IAM role ARN used to apply this mitigation action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maRoleARN :: Lens.Lens' MitigationAction (Lude.Maybe Lude.Text)
maRoleARN = Lens.lens (roleARN :: MitigationAction -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: MitigationAction)
{-# DEPRECATED maRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON MitigationAction where
  parseJSON =
    Lude.withObject
      "MitigationAction"
      ( \x ->
          MitigationAction'
            Lude.<$> (x Lude..:? "actionParams")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "roleArn")
      )

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationActionIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationActionIdentifier
  ( MitigationActionIdentifier (..),

    -- * Smart constructor
    mkMitigationActionIdentifier,

    -- * Lenses
    maiActionName,
    maiCreationDate,
    maiActionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information that identifies a mitigation action. This information is returned by ListMitigationActions.
--
-- /See:/ 'mkMitigationActionIdentifier' smart constructor.
data MitigationActionIdentifier = MitigationActionIdentifier'
  { actionName ::
      Lude.Maybe Lude.Text,
    creationDate ::
      Lude.Maybe Lude.Timestamp,
    actionARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MitigationActionIdentifier' with the minimum fields required to make a request.
--
-- * 'actionARN' - The IAM role ARN used to apply this mitigation action.
-- * 'actionName' - The friendly name of the mitigation action.
-- * 'creationDate' - The date when this mitigation action was created.
mkMitigationActionIdentifier ::
  MitigationActionIdentifier
mkMitigationActionIdentifier =
  MitigationActionIdentifier'
    { actionName = Lude.Nothing,
      creationDate = Lude.Nothing,
      actionARN = Lude.Nothing
    }

-- | The friendly name of the mitigation action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maiActionName :: Lens.Lens' MitigationActionIdentifier (Lude.Maybe Lude.Text)
maiActionName = Lens.lens (actionName :: MitigationActionIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: MitigationActionIdentifier)
{-# DEPRECATED maiActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The date when this mitigation action was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maiCreationDate :: Lens.Lens' MitigationActionIdentifier (Lude.Maybe Lude.Timestamp)
maiCreationDate = Lens.lens (creationDate :: MitigationActionIdentifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: MitigationActionIdentifier)
{-# DEPRECATED maiCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The IAM role ARN used to apply this mitigation action.
--
-- /Note:/ Consider using 'actionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maiActionARN :: Lens.Lens' MitigationActionIdentifier (Lude.Maybe Lude.Text)
maiActionARN = Lens.lens (actionARN :: MitigationActionIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {actionARN = a} :: MitigationActionIdentifier)
{-# DEPRECATED maiActionARN "Use generic-lens or generic-optics with 'actionARN' instead." #-}

instance Lude.FromJSON MitigationActionIdentifier where
  parseJSON =
    Lude.withObject
      "MitigationActionIdentifier"
      ( \x ->
          MitigationActionIdentifier'
            Lude.<$> (x Lude..:? "actionName")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "actionArn")
      )

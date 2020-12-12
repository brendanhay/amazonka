{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionType
  ( ActionType (..),

    -- * Smart constructor
    mkActionType,

    -- * Lenses
    atSettings,
    atActionConfigurationProperties,
    atId,
    atInputArtifactDetails,
    atOutputArtifactDetails,
  )
where

import Network.AWS.CodePipeline.Types.ActionConfigurationProperty
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ActionTypeSettings
import Network.AWS.CodePipeline.Types.ArtifactDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the details of an action type.
--
-- /See:/ 'mkActionType' smart constructor.
data ActionType = ActionType'
  { settings ::
      Lude.Maybe ActionTypeSettings,
    actionConfigurationProperties ::
      Lude.Maybe [ActionConfigurationProperty],
    id :: ActionTypeId,
    inputArtifactDetails :: ArtifactDetails,
    outputArtifactDetails :: ArtifactDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionType' with the minimum fields required to make a request.
--
-- * 'actionConfigurationProperties' - The configuration properties for the action type.
-- * 'id' - Represents information about an action type.
-- * 'inputArtifactDetails' - The details of the input artifact for the action, such as its commit ID.
-- * 'outputArtifactDetails' - The details of the output artifact of the action, such as its commit ID.
-- * 'settings' - The settings for the action type.
mkActionType ::
  -- | 'id'
  ActionTypeId ->
  -- | 'inputArtifactDetails'
  ArtifactDetails ->
  -- | 'outputArtifactDetails'
  ArtifactDetails ->
  ActionType
mkActionType pId_ pInputArtifactDetails_ pOutputArtifactDetails_ =
  ActionType'
    { settings = Lude.Nothing,
      actionConfigurationProperties = Lude.Nothing,
      id = pId_,
      inputArtifactDetails = pInputArtifactDetails_,
      outputArtifactDetails = pOutputArtifactDetails_
    }

-- | The settings for the action type.
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atSettings :: Lens.Lens' ActionType (Lude.Maybe ActionTypeSettings)
atSettings = Lens.lens (settings :: ActionType -> Lude.Maybe ActionTypeSettings) (\s a -> s {settings = a} :: ActionType)
{-# DEPRECATED atSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The configuration properties for the action type.
--
-- /Note:/ Consider using 'actionConfigurationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atActionConfigurationProperties :: Lens.Lens' ActionType (Lude.Maybe [ActionConfigurationProperty])
atActionConfigurationProperties = Lens.lens (actionConfigurationProperties :: ActionType -> Lude.Maybe [ActionConfigurationProperty]) (\s a -> s {actionConfigurationProperties = a} :: ActionType)
{-# DEPRECATED atActionConfigurationProperties "Use generic-lens or generic-optics with 'actionConfigurationProperties' instead." #-}

-- | Represents information about an action type.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atId :: Lens.Lens' ActionType ActionTypeId
atId = Lens.lens (id :: ActionType -> ActionTypeId) (\s a -> s {id = a} :: ActionType)
{-# DEPRECATED atId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The details of the input artifact for the action, such as its commit ID.
--
-- /Note:/ Consider using 'inputArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atInputArtifactDetails :: Lens.Lens' ActionType ArtifactDetails
atInputArtifactDetails = Lens.lens (inputArtifactDetails :: ActionType -> ArtifactDetails) (\s a -> s {inputArtifactDetails = a} :: ActionType)
{-# DEPRECATED atInputArtifactDetails "Use generic-lens or generic-optics with 'inputArtifactDetails' instead." #-}

-- | The details of the output artifact of the action, such as its commit ID.
--
-- /Note:/ Consider using 'outputArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atOutputArtifactDetails :: Lens.Lens' ActionType ArtifactDetails
atOutputArtifactDetails = Lens.lens (outputArtifactDetails :: ActionType -> ArtifactDetails) (\s a -> s {outputArtifactDetails = a} :: ActionType)
{-# DEPRECATED atOutputArtifactDetails "Use generic-lens or generic-optics with 'outputArtifactDetails' instead." #-}

instance Lude.FromJSON ActionType where
  parseJSON =
    Lude.withObject
      "ActionType"
      ( \x ->
          ActionType'
            Lude.<$> (x Lude..:? "settings")
            Lude.<*> (x Lude..:? "actionConfigurationProperties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "id")
            Lude.<*> (x Lude..: "inputArtifactDetails")
            Lude.<*> (x Lude..: "outputArtifactDetails")
      )

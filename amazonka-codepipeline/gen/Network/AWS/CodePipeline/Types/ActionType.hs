{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionType where

import Network.AWS.CodePipeline.Types.ActionConfigurationProperty
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ActionTypeSettings
import Network.AWS.CodePipeline.Types.ArtifactDetails
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about the details of an action type.
--
-- /See:/ 'newActionType' smart constructor.
data ActionType = ActionType'
  { -- | The configuration properties for the action type.
    actionConfigurationProperties :: Core.Maybe [ActionConfigurationProperty],
    -- | The settings for the action type.
    settings :: Core.Maybe ActionTypeSettings,
    -- | Represents information about an action type.
    id :: ActionTypeId,
    -- | The details of the input artifact for the action, such as its commit ID.
    inputArtifactDetails :: ArtifactDetails,
    -- | The details of the output artifact of the action, such as its commit ID.
    outputArtifactDetails :: ArtifactDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionConfigurationProperties', 'actionType_actionConfigurationProperties' - The configuration properties for the action type.
--
-- 'settings', 'actionType_settings' - The settings for the action type.
--
-- 'id', 'actionType_id' - Represents information about an action type.
--
-- 'inputArtifactDetails', 'actionType_inputArtifactDetails' - The details of the input artifact for the action, such as its commit ID.
--
-- 'outputArtifactDetails', 'actionType_outputArtifactDetails' - The details of the output artifact of the action, such as its commit ID.
newActionType ::
  -- | 'id'
  ActionTypeId ->
  -- | 'inputArtifactDetails'
  ArtifactDetails ->
  -- | 'outputArtifactDetails'
  ArtifactDetails ->
  ActionType
newActionType
  pId_
  pInputArtifactDetails_
  pOutputArtifactDetails_ =
    ActionType'
      { actionConfigurationProperties =
          Core.Nothing,
        settings = Core.Nothing,
        id = pId_,
        inputArtifactDetails = pInputArtifactDetails_,
        outputArtifactDetails = pOutputArtifactDetails_
      }

-- | The configuration properties for the action type.
actionType_actionConfigurationProperties :: Lens.Lens' ActionType (Core.Maybe [ActionConfigurationProperty])
actionType_actionConfigurationProperties = Lens.lens (\ActionType' {actionConfigurationProperties} -> actionConfigurationProperties) (\s@ActionType' {} a -> s {actionConfigurationProperties = a} :: ActionType) Core.. Lens.mapping Lens._Coerce

-- | The settings for the action type.
actionType_settings :: Lens.Lens' ActionType (Core.Maybe ActionTypeSettings)
actionType_settings = Lens.lens (\ActionType' {settings} -> settings) (\s@ActionType' {} a -> s {settings = a} :: ActionType)

-- | Represents information about an action type.
actionType_id :: Lens.Lens' ActionType ActionTypeId
actionType_id = Lens.lens (\ActionType' {id} -> id) (\s@ActionType' {} a -> s {id = a} :: ActionType)

-- | The details of the input artifact for the action, such as its commit ID.
actionType_inputArtifactDetails :: Lens.Lens' ActionType ArtifactDetails
actionType_inputArtifactDetails = Lens.lens (\ActionType' {inputArtifactDetails} -> inputArtifactDetails) (\s@ActionType' {} a -> s {inputArtifactDetails = a} :: ActionType)

-- | The details of the output artifact of the action, such as its commit ID.
actionType_outputArtifactDetails :: Lens.Lens' ActionType ArtifactDetails
actionType_outputArtifactDetails = Lens.lens (\ActionType' {outputArtifactDetails} -> outputArtifactDetails) (\s@ActionType' {} a -> s {outputArtifactDetails = a} :: ActionType)

instance Core.FromJSON ActionType where
  parseJSON =
    Core.withObject
      "ActionType"
      ( \x ->
          ActionType'
            Core.<$> ( x Core..:? "actionConfigurationProperties"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "settings")
            Core.<*> (x Core..: "id")
            Core.<*> (x Core..: "inputArtifactDetails")
            Core.<*> (x Core..: "outputArtifactDetails")
      )

instance Core.Hashable ActionType

instance Core.NFData ActionType

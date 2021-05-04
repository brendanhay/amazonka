{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about the details of an action type.
--
-- /See:/ 'newActionType' smart constructor.
data ActionType = ActionType'
  { -- | The configuration properties for the action type.
    actionConfigurationProperties :: Prelude.Maybe [ActionConfigurationProperty],
    -- | The settings for the action type.
    settings :: Prelude.Maybe ActionTypeSettings,
    -- | Represents information about an action type.
    id :: ActionTypeId,
    -- | The details of the input artifact for the action, such as its commit ID.
    inputArtifactDetails :: ArtifactDetails,
    -- | The details of the output artifact of the action, such as its commit ID.
    outputArtifactDetails :: ArtifactDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
          Prelude.Nothing,
        settings = Prelude.Nothing,
        id = pId_,
        inputArtifactDetails = pInputArtifactDetails_,
        outputArtifactDetails = pOutputArtifactDetails_
      }

-- | The configuration properties for the action type.
actionType_actionConfigurationProperties :: Lens.Lens' ActionType (Prelude.Maybe [ActionConfigurationProperty])
actionType_actionConfigurationProperties = Lens.lens (\ActionType' {actionConfigurationProperties} -> actionConfigurationProperties) (\s@ActionType' {} a -> s {actionConfigurationProperties = a} :: ActionType) Prelude.. Lens.mapping Prelude._Coerce

-- | The settings for the action type.
actionType_settings :: Lens.Lens' ActionType (Prelude.Maybe ActionTypeSettings)
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

instance Prelude.FromJSON ActionType where
  parseJSON =
    Prelude.withObject
      "ActionType"
      ( \x ->
          ActionType'
            Prelude.<$> ( x Prelude..:? "actionConfigurationProperties"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "settings")
            Prelude.<*> (x Prelude..: "id")
            Prelude.<*> (x Prelude..: "inputArtifactDetails")
            Prelude.<*> (x Prelude..: "outputArtifactDetails")
      )

instance Prelude.Hashable ActionType

instance Prelude.NFData ActionType

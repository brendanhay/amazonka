{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionType where

import Network.AWS.CodePipeline.Types.ActionConfigurationProperty
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ActionTypeSettings
import Network.AWS.CodePipeline.Types.ArtifactDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about the details of an action type.
--
--
--
-- /See:/ 'actionType' smart constructor.
data ActionType = ActionType'
  { _atSettings ::
      !(Maybe ActionTypeSettings),
    _atActionConfigurationProperties ::
      !(Maybe [ActionConfigurationProperty]),
    _atId :: !ActionTypeId,
    _atInputArtifactDetails :: !ArtifactDetails,
    _atOutputArtifactDetails :: !ArtifactDetails
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atSettings' - The settings for the action type.
--
-- * 'atActionConfigurationProperties' - The configuration properties for the action type.
--
-- * 'atId' - Represents information about an action type.
--
-- * 'atInputArtifactDetails' - The details of the input artifact for the action, such as its commit ID.
--
-- * 'atOutputArtifactDetails' - The details of the output artifact of the action, such as its commit ID.
actionType ::
  -- | 'atId'
  ActionTypeId ->
  -- | 'atInputArtifactDetails'
  ArtifactDetails ->
  -- | 'atOutputArtifactDetails'
  ArtifactDetails ->
  ActionType
actionType pId_ pInputArtifactDetails_ pOutputArtifactDetails_ =
  ActionType'
    { _atSettings = Nothing,
      _atActionConfigurationProperties = Nothing,
      _atId = pId_,
      _atInputArtifactDetails = pInputArtifactDetails_,
      _atOutputArtifactDetails = pOutputArtifactDetails_
    }

-- | The settings for the action type.
atSettings :: Lens' ActionType (Maybe ActionTypeSettings)
atSettings = lens _atSettings (\s a -> s {_atSettings = a})

-- | The configuration properties for the action type.
atActionConfigurationProperties :: Lens' ActionType [ActionConfigurationProperty]
atActionConfigurationProperties = lens _atActionConfigurationProperties (\s a -> s {_atActionConfigurationProperties = a}) . _Default . _Coerce

-- | Represents information about an action type.
atId :: Lens' ActionType ActionTypeId
atId = lens _atId (\s a -> s {_atId = a})

-- | The details of the input artifact for the action, such as its commit ID.
atInputArtifactDetails :: Lens' ActionType ArtifactDetails
atInputArtifactDetails = lens _atInputArtifactDetails (\s a -> s {_atInputArtifactDetails = a})

-- | The details of the output artifact of the action, such as its commit ID.
atOutputArtifactDetails :: Lens' ActionType ArtifactDetails
atOutputArtifactDetails = lens _atOutputArtifactDetails (\s a -> s {_atOutputArtifactDetails = a})

instance FromJSON ActionType where
  parseJSON =
    withObject
      "ActionType"
      ( \x ->
          ActionType'
            <$> (x .:? "settings")
            <*> (x .:? "actionConfigurationProperties" .!= mempty)
            <*> (x .: "id")
            <*> (x .: "inputArtifactDetails")
            <*> (x .: "outputArtifactDetails")
      )

instance Hashable ActionType

instance NFData ActionType

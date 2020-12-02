{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeId where

import Network.AWS.CodePipeline.Types.ActionCategory
import Network.AWS.CodePipeline.Types.ActionOwner
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about an action type.
--
--
--
-- /See:/ 'actionTypeId' smart constructor.
data ActionTypeId = ActionTypeId'
  { _atiCategory :: !ActionCategory,
    _atiOwner :: !ActionOwner,
    _atiProvider :: !Text,
    _atiVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionTypeId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atiCategory' - A category defines what kind of action can be taken in the stage, and constrains the provider type for the action. Valid categories are limited to one of the following values.      * Source     * Build     * Test     * Deploy     * Invoke     * Approval
--
-- * 'atiOwner' - The creator of the action being called. There are three valid values for the @Owner@ field in the action category section within your pipeline structure: @AWS@ , @ThirdParty@ , and @Custom@ . For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
--
-- * 'atiProvider' - The provider of the service being called by the action. Valid providers are determined by the action category. For example, an action in the Deploy category type might have a provider of AWS CodeDeploy, which would be specified as CodeDeploy. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
--
-- * 'atiVersion' - A string that describes the action version.
actionTypeId ::
  -- | 'atiCategory'
  ActionCategory ->
  -- | 'atiOwner'
  ActionOwner ->
  -- | 'atiProvider'
  Text ->
  -- | 'atiVersion'
  Text ->
  ActionTypeId
actionTypeId pCategory_ pOwner_ pProvider_ pVersion_ =
  ActionTypeId'
    { _atiCategory = pCategory_,
      _atiOwner = pOwner_,
      _atiProvider = pProvider_,
      _atiVersion = pVersion_
    }

-- | A category defines what kind of action can be taken in the stage, and constrains the provider type for the action. Valid categories are limited to one of the following values.      * Source     * Build     * Test     * Deploy     * Invoke     * Approval
atiCategory :: Lens' ActionTypeId ActionCategory
atiCategory = lens _atiCategory (\s a -> s {_atiCategory = a})

-- | The creator of the action being called. There are three valid values for the @Owner@ field in the action category section within your pipeline structure: @AWS@ , @ThirdParty@ , and @Custom@ . For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
atiOwner :: Lens' ActionTypeId ActionOwner
atiOwner = lens _atiOwner (\s a -> s {_atiOwner = a})

-- | The provider of the service being called by the action. Valid providers are determined by the action category. For example, an action in the Deploy category type might have a provider of AWS CodeDeploy, which would be specified as CodeDeploy. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#actions-valid-providers Valid Action Types and Providers in CodePipeline> .
atiProvider :: Lens' ActionTypeId Text
atiProvider = lens _atiProvider (\s a -> s {_atiProvider = a})

-- | A string that describes the action version.
atiVersion :: Lens' ActionTypeId Text
atiVersion = lens _atiVersion (\s a -> s {_atiVersion = a})

instance FromJSON ActionTypeId where
  parseJSON =
    withObject
      "ActionTypeId"
      ( \x ->
          ActionTypeId'
            <$> (x .: "category")
            <*> (x .: "owner")
            <*> (x .: "provider")
            <*> (x .: "version")
      )

instance Hashable ActionTypeId

instance NFData ActionTypeId

instance ToJSON ActionTypeId where
  toJSON ActionTypeId' {..} =
    object
      ( catMaybes
          [ Just ("category" .= _atiCategory),
            Just ("owner" .= _atiOwner),
            Just ("provider" .= _atiProvider),
            Just ("version" .= _atiVersion)
          ]
      )

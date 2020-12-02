{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionDeclaration where

import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.InputArtifact
import Network.AWS.CodePipeline.Types.OutputArtifact
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about an action declaration.
--
--
--
-- /See:/ 'actionDeclaration' smart constructor.
data ActionDeclaration = ActionDeclaration'
  { _adOutputArtifacts ::
      !(Maybe [OutputArtifact]),
    _adNamespace :: !(Maybe Text),
    _adRunOrder :: !(Maybe Nat),
    _adRegion :: !(Maybe Text),
    _adConfiguration :: !(Maybe (Map Text (Text))),
    _adInputArtifacts :: !(Maybe [InputArtifact]),
    _adRoleARN :: !(Maybe Text),
    _adName :: !Text,
    _adActionTypeId :: !ActionTypeId
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionDeclaration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adOutputArtifacts' - The name or ID of the result of the action declaration, such as a test or build artifact.
--
-- * 'adNamespace' - The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
--
-- * 'adRunOrder' - The order in which actions are run.
--
-- * 'adRegion' - The action declaration's AWS Region, such as us-east-1.
--
-- * 'adConfiguration' - The action's configuration. These are key-value pairs that specify input values for an action. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline> . For the list of configuration properties for the AWS CloudFormation action type in CodePipeline, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference> in the /AWS CloudFormation User Guide/ . For template snippets with examples, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines> in the /AWS CloudFormation User Guide/ . The values can be represented in either JSON or YAML format. For example, the JSON configuration item format is as follows:  /JSON:/  @"Configuration" : { Key : Value },@
--
-- * 'adInputArtifacts' - The name or ID of the artifact consumed by the action, such as a test or build artifact.
--
-- * 'adRoleARN' - The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
--
-- * 'adName' - The action declaration's name.
--
-- * 'adActionTypeId' - Specifies the action type and the provider of the action.
actionDeclaration ::
  -- | 'adName'
  Text ->
  -- | 'adActionTypeId'
  ActionTypeId ->
  ActionDeclaration
actionDeclaration pName_ pActionTypeId_ =
  ActionDeclaration'
    { _adOutputArtifacts = Nothing,
      _adNamespace = Nothing,
      _adRunOrder = Nothing,
      _adRegion = Nothing,
      _adConfiguration = Nothing,
      _adInputArtifacts = Nothing,
      _adRoleARN = Nothing,
      _adName = pName_,
      _adActionTypeId = pActionTypeId_
    }

-- | The name or ID of the result of the action declaration, such as a test or build artifact.
adOutputArtifacts :: Lens' ActionDeclaration [OutputArtifact]
adOutputArtifacts = lens _adOutputArtifacts (\s a -> s {_adOutputArtifacts = a}) . _Default . _Coerce

-- | The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
adNamespace :: Lens' ActionDeclaration (Maybe Text)
adNamespace = lens _adNamespace (\s a -> s {_adNamespace = a})

-- | The order in which actions are run.
adRunOrder :: Lens' ActionDeclaration (Maybe Natural)
adRunOrder = lens _adRunOrder (\s a -> s {_adRunOrder = a}) . mapping _Nat

-- | The action declaration's AWS Region, such as us-east-1.
adRegion :: Lens' ActionDeclaration (Maybe Text)
adRegion = lens _adRegion (\s a -> s {_adRegion = a})

-- | The action's configuration. These are key-value pairs that specify input values for an action. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/reference-pipeline-structure.html#action-requirements Action Structure Requirements in CodePipeline> . For the list of configuration properties for the AWS CloudFormation action type in CodePipeline, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-action-reference.html Configuration Properties Reference> in the /AWS CloudFormation User Guide/ . For template snippets with examples, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/continuous-delivery-codepipeline-parameter-override-functions.html Using Parameter Override Functions with CodePipeline Pipelines> in the /AWS CloudFormation User Guide/ . The values can be represented in either JSON or YAML format. For example, the JSON configuration item format is as follows:  /JSON:/  @"Configuration" : { Key : Value },@
adConfiguration :: Lens' ActionDeclaration (HashMap Text (Text))
adConfiguration = lens _adConfiguration (\s a -> s {_adConfiguration = a}) . _Default . _Map

-- | The name or ID of the artifact consumed by the action, such as a test or build artifact.
adInputArtifacts :: Lens' ActionDeclaration [InputArtifact]
adInputArtifacts = lens _adInputArtifacts (\s a -> s {_adInputArtifacts = a}) . _Default . _Coerce

-- | The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
adRoleARN :: Lens' ActionDeclaration (Maybe Text)
adRoleARN = lens _adRoleARN (\s a -> s {_adRoleARN = a})

-- | The action declaration's name.
adName :: Lens' ActionDeclaration Text
adName = lens _adName (\s a -> s {_adName = a})

-- | Specifies the action type and the provider of the action.
adActionTypeId :: Lens' ActionDeclaration ActionTypeId
adActionTypeId = lens _adActionTypeId (\s a -> s {_adActionTypeId = a})

instance FromJSON ActionDeclaration where
  parseJSON =
    withObject
      "ActionDeclaration"
      ( \x ->
          ActionDeclaration'
            <$> (x .:? "outputArtifacts" .!= mempty)
            <*> (x .:? "namespace")
            <*> (x .:? "runOrder")
            <*> (x .:? "region")
            <*> (x .:? "configuration" .!= mempty)
            <*> (x .:? "inputArtifacts" .!= mempty)
            <*> (x .:? "roleArn")
            <*> (x .: "name")
            <*> (x .: "actionTypeId")
      )

instance Hashable ActionDeclaration

instance NFData ActionDeclaration

instance ToJSON ActionDeclaration where
  toJSON ActionDeclaration' {..} =
    object
      ( catMaybes
          [ ("outputArtifacts" .=) <$> _adOutputArtifacts,
            ("namespace" .=) <$> _adNamespace,
            ("runOrder" .=) <$> _adRunOrder,
            ("region" .=) <$> _adRegion,
            ("configuration" .=) <$> _adConfiguration,
            ("inputArtifacts" .=) <$> _adInputArtifacts,
            ("roleArn" .=) <$> _adRoleARN,
            Just ("name" .= _adName),
            Just ("actionTypeId" .= _adActionTypeId)
          ]
      )

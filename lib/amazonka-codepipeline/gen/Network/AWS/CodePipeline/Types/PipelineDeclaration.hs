{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineDeclaration where

import Network.AWS.CodePipeline.Types.ArtifactStore
import Network.AWS.CodePipeline.Types.StageDeclaration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the structure of actions and stages to be performed in the pipeline.
--
--
--
-- /See:/ 'pipelineDeclaration' smart constructor.
data PipelineDeclaration = PipelineDeclaration'
  { _pdArtifactStores ::
      !(Maybe (Map Text (ArtifactStore))),
    _pdArtifactStore :: !(Maybe ArtifactStore),
    _pdVersion :: !(Maybe Nat),
    _pdName :: !Text,
    _pdRoleARN :: !Text,
    _pdStages :: ![StageDeclaration]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineDeclaration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdArtifactStores' - A mapping of @artifactStore@ objects and their corresponding AWS Regions. There must be an artifact store for the pipeline Region and for each cross-region action in the pipeline.
--
-- * 'pdArtifactStore' - Represents information about the S3 bucket where artifacts are stored for the pipeline.
--
-- * 'pdVersion' - The version number of the pipeline. A new pipeline always has a version number of 1. This number is incremented when a pipeline is updated.
--
-- * 'pdName' - The name of the pipeline.
--
-- * 'pdRoleARN' - The Amazon Resource Name (ARN) for AWS CodePipeline to use to either perform actions with no @actionRoleArn@ , or to use to assume roles for actions with an @actionRoleArn@ .
--
-- * 'pdStages' - The stage in which to perform the action.
pipelineDeclaration ::
  -- | 'pdName'
  Text ->
  -- | 'pdRoleARN'
  Text ->
  PipelineDeclaration
pipelineDeclaration pName_ pRoleARN_ =
  PipelineDeclaration'
    { _pdArtifactStores = Nothing,
      _pdArtifactStore = Nothing,
      _pdVersion = Nothing,
      _pdName = pName_,
      _pdRoleARN = pRoleARN_,
      _pdStages = mempty
    }

-- | A mapping of @artifactStore@ objects and their corresponding AWS Regions. There must be an artifact store for the pipeline Region and for each cross-region action in the pipeline.
pdArtifactStores :: Lens' PipelineDeclaration (HashMap Text (ArtifactStore))
pdArtifactStores = lens _pdArtifactStores (\s a -> s {_pdArtifactStores = a}) . _Default . _Map

-- | Represents information about the S3 bucket where artifacts are stored for the pipeline.
pdArtifactStore :: Lens' PipelineDeclaration (Maybe ArtifactStore)
pdArtifactStore = lens _pdArtifactStore (\s a -> s {_pdArtifactStore = a})

-- | The version number of the pipeline. A new pipeline always has a version number of 1. This number is incremented when a pipeline is updated.
pdVersion :: Lens' PipelineDeclaration (Maybe Natural)
pdVersion = lens _pdVersion (\s a -> s {_pdVersion = a}) . mapping _Nat

-- | The name of the pipeline.
pdName :: Lens' PipelineDeclaration Text
pdName = lens _pdName (\s a -> s {_pdName = a})

-- | The Amazon Resource Name (ARN) for AWS CodePipeline to use to either perform actions with no @actionRoleArn@ , or to use to assume roles for actions with an @actionRoleArn@ .
pdRoleARN :: Lens' PipelineDeclaration Text
pdRoleARN = lens _pdRoleARN (\s a -> s {_pdRoleARN = a})

-- | The stage in which to perform the action.
pdStages :: Lens' PipelineDeclaration [StageDeclaration]
pdStages = lens _pdStages (\s a -> s {_pdStages = a}) . _Coerce

instance FromJSON PipelineDeclaration where
  parseJSON =
    withObject
      "PipelineDeclaration"
      ( \x ->
          PipelineDeclaration'
            <$> (x .:? "artifactStores" .!= mempty)
            <*> (x .:? "artifactStore")
            <*> (x .:? "version")
            <*> (x .: "name")
            <*> (x .: "roleArn")
            <*> (x .:? "stages" .!= mempty)
      )

instance Hashable PipelineDeclaration

instance NFData PipelineDeclaration

instance ToJSON PipelineDeclaration where
  toJSON PipelineDeclaration' {..} =
    object
      ( catMaybes
          [ ("artifactStores" .=) <$> _pdArtifactStores,
            ("artifactStore" .=) <$> _pdArtifactStore,
            ("version" .=) <$> _pdVersion,
            Just ("name" .= _pdName),
            Just ("roleArn" .= _pdRoleARN),
            Just ("stages" .= _pdStages)
          ]
      )

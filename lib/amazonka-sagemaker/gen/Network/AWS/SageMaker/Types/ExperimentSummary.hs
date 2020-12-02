{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ExperimentSource

-- | A summary of the properties of an experiment. To get the complete set of properties, call the 'DescribeExperiment' API and provide the @ExperimentName@ .
--
--
--
-- /See:/ 'experimentSummary' smart constructor.
data ExperimentSummary = ExperimentSummary'
  { _expCreationTime ::
      !(Maybe POSIX),
    _expLastModifiedTime :: !(Maybe POSIX),
    _expExperimentName :: !(Maybe Text),
    _expExperimentSource :: !(Maybe ExperimentSource),
    _expExperimentARN :: !(Maybe Text),
    _expDisplayName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExperimentSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'expCreationTime' - When the experiment was created.
--
-- * 'expLastModifiedTime' - When the experiment was last modified.
--
-- * 'expExperimentName' - The name of the experiment.
--
-- * 'expExperimentSource' - Undocumented member.
--
-- * 'expExperimentARN' - The Amazon Resource Name (ARN) of the experiment.
--
-- * 'expDisplayName' - The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
experimentSummary ::
  ExperimentSummary
experimentSummary =
  ExperimentSummary'
    { _expCreationTime = Nothing,
      _expLastModifiedTime = Nothing,
      _expExperimentName = Nothing,
      _expExperimentSource = Nothing,
      _expExperimentARN = Nothing,
      _expDisplayName = Nothing
    }

-- | When the experiment was created.
expCreationTime :: Lens' ExperimentSummary (Maybe UTCTime)
expCreationTime = lens _expCreationTime (\s a -> s {_expCreationTime = a}) . mapping _Time

-- | When the experiment was last modified.
expLastModifiedTime :: Lens' ExperimentSummary (Maybe UTCTime)
expLastModifiedTime = lens _expLastModifiedTime (\s a -> s {_expLastModifiedTime = a}) . mapping _Time

-- | The name of the experiment.
expExperimentName :: Lens' ExperimentSummary (Maybe Text)
expExperimentName = lens _expExperimentName (\s a -> s {_expExperimentName = a})

-- | Undocumented member.
expExperimentSource :: Lens' ExperimentSummary (Maybe ExperimentSource)
expExperimentSource = lens _expExperimentSource (\s a -> s {_expExperimentSource = a})

-- | The Amazon Resource Name (ARN) of the experiment.
expExperimentARN :: Lens' ExperimentSummary (Maybe Text)
expExperimentARN = lens _expExperimentARN (\s a -> s {_expExperimentARN = a})

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
expDisplayName :: Lens' ExperimentSummary (Maybe Text)
expDisplayName = lens _expDisplayName (\s a -> s {_expDisplayName = a})

instance FromJSON ExperimentSummary where
  parseJSON =
    withObject
      "ExperimentSummary"
      ( \x ->
          ExperimentSummary'
            <$> (x .:? "CreationTime")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "ExperimentName")
            <*> (x .:? "ExperimentSource")
            <*> (x .:? "ExperimentArn")
            <*> (x .:? "DisplayName")
      )

instance Hashable ExperimentSummary

instance NFData ExperimentSummary

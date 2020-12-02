{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Experiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Experiment where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ExperimentSource
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of an experiment as returned by the 'Search' API.
--
--
--
-- /See:/ 'experiment' smart constructor.
data Experiment = Experiment'
  { _eCreationTime :: !(Maybe POSIX),
    _eCreatedBy :: !(Maybe UserContext),
    _eLastModifiedTime :: !(Maybe POSIX),
    _eExperimentName :: !(Maybe Text),
    _eExperimentARN :: !(Maybe Text),
    _eSource :: !(Maybe ExperimentSource),
    _eDisplayName :: !(Maybe Text),
    _eLastModifiedBy :: !(Maybe UserContext),
    _eDescription :: !(Maybe Text),
    _eTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Experiment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eCreationTime' - When the experiment was created.
--
-- * 'eCreatedBy' - Undocumented member.
--
-- * 'eLastModifiedTime' - When the experiment was last modified.
--
-- * 'eExperimentName' - The name of the experiment.
--
-- * 'eExperimentARN' - The Amazon Resource Name (ARN) of the experiment.
--
-- * 'eSource' - Undocumented member.
--
-- * 'eDisplayName' - The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- * 'eLastModifiedBy' - Undocumented member.
--
-- * 'eDescription' - The description of the experiment.
--
-- * 'eTags' - The list of tags that are associated with the experiment. You can use 'Search' API to search on the tags.
experiment ::
  Experiment
experiment =
  Experiment'
    { _eCreationTime = Nothing,
      _eCreatedBy = Nothing,
      _eLastModifiedTime = Nothing,
      _eExperimentName = Nothing,
      _eExperimentARN = Nothing,
      _eSource = Nothing,
      _eDisplayName = Nothing,
      _eLastModifiedBy = Nothing,
      _eDescription = Nothing,
      _eTags = Nothing
    }

-- | When the experiment was created.
eCreationTime :: Lens' Experiment (Maybe UTCTime)
eCreationTime = lens _eCreationTime (\s a -> s {_eCreationTime = a}) . mapping _Time

-- | Undocumented member.
eCreatedBy :: Lens' Experiment (Maybe UserContext)
eCreatedBy = lens _eCreatedBy (\s a -> s {_eCreatedBy = a})

-- | When the experiment was last modified.
eLastModifiedTime :: Lens' Experiment (Maybe UTCTime)
eLastModifiedTime = lens _eLastModifiedTime (\s a -> s {_eLastModifiedTime = a}) . mapping _Time

-- | The name of the experiment.
eExperimentName :: Lens' Experiment (Maybe Text)
eExperimentName = lens _eExperimentName (\s a -> s {_eExperimentName = a})

-- | The Amazon Resource Name (ARN) of the experiment.
eExperimentARN :: Lens' Experiment (Maybe Text)
eExperimentARN = lens _eExperimentARN (\s a -> s {_eExperimentARN = a})

-- | Undocumented member.
eSource :: Lens' Experiment (Maybe ExperimentSource)
eSource = lens _eSource (\s a -> s {_eSource = a})

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
eDisplayName :: Lens' Experiment (Maybe Text)
eDisplayName = lens _eDisplayName (\s a -> s {_eDisplayName = a})

-- | Undocumented member.
eLastModifiedBy :: Lens' Experiment (Maybe UserContext)
eLastModifiedBy = lens _eLastModifiedBy (\s a -> s {_eLastModifiedBy = a})

-- | The description of the experiment.
eDescription :: Lens' Experiment (Maybe Text)
eDescription = lens _eDescription (\s a -> s {_eDescription = a})

-- | The list of tags that are associated with the experiment. You can use 'Search' API to search on the tags.
eTags :: Lens' Experiment [Tag]
eTags = lens _eTags (\s a -> s {_eTags = a}) . _Default . _Coerce

instance FromJSON Experiment where
  parseJSON =
    withObject
      "Experiment"
      ( \x ->
          Experiment'
            <$> (x .:? "CreationTime")
            <*> (x .:? "CreatedBy")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "ExperimentName")
            <*> (x .:? "ExperimentArn")
            <*> (x .:? "Source")
            <*> (x .:? "DisplayName")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "Description")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable Experiment

instance NFData Experiment

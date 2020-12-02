{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Trial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Trial where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
import Network.AWS.SageMaker.Types.TrialSource
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of a trial as returned by the 'Search' API.
--
--
--
-- /See:/ 'trial' smart constructor.
data Trial = Trial'
  { _tCreationTime :: !(Maybe POSIX),
    _tTrialComponentSummaries ::
      !(Maybe [TrialComponentSimpleSummary]),
    _tTrialARN :: !(Maybe Text),
    _tCreatedBy :: !(Maybe UserContext),
    _tLastModifiedTime :: !(Maybe POSIX),
    _tExperimentName :: !(Maybe Text),
    _tSource :: !(Maybe TrialSource),
    _tDisplayName :: !(Maybe Text),
    _tTrialName :: !(Maybe Text),
    _tLastModifiedBy :: !(Maybe UserContext),
    _tTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Trial' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tCreationTime' - When the trial was created.
--
-- * 'tTrialComponentSummaries' - A list of the components associated with the trial. For each component, a summary of the component's properties is included.
--
-- * 'tTrialARN' - The Amazon Resource Name (ARN) of the trial.
--
-- * 'tCreatedBy' - Undocumented member.
--
-- * 'tLastModifiedTime' - Who last modified the trial.
--
-- * 'tExperimentName' - The name of the experiment the trial is part of.
--
-- * 'tSource' - Undocumented member.
--
-- * 'tDisplayName' - The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- * 'tTrialName' - The name of the trial.
--
-- * 'tLastModifiedBy' - Undocumented member.
--
-- * 'tTags' - The list of tags that are associated with the trial. You can use 'Search' API to search on the tags.
trial ::
  Trial
trial =
  Trial'
    { _tCreationTime = Nothing,
      _tTrialComponentSummaries = Nothing,
      _tTrialARN = Nothing,
      _tCreatedBy = Nothing,
      _tLastModifiedTime = Nothing,
      _tExperimentName = Nothing,
      _tSource = Nothing,
      _tDisplayName = Nothing,
      _tTrialName = Nothing,
      _tLastModifiedBy = Nothing,
      _tTags = Nothing
    }

-- | When the trial was created.
tCreationTime :: Lens' Trial (Maybe UTCTime)
tCreationTime = lens _tCreationTime (\s a -> s {_tCreationTime = a}) . mapping _Time

-- | A list of the components associated with the trial. For each component, a summary of the component's properties is included.
tTrialComponentSummaries :: Lens' Trial [TrialComponentSimpleSummary]
tTrialComponentSummaries = lens _tTrialComponentSummaries (\s a -> s {_tTrialComponentSummaries = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the trial.
tTrialARN :: Lens' Trial (Maybe Text)
tTrialARN = lens _tTrialARN (\s a -> s {_tTrialARN = a})

-- | Undocumented member.
tCreatedBy :: Lens' Trial (Maybe UserContext)
tCreatedBy = lens _tCreatedBy (\s a -> s {_tCreatedBy = a})

-- | Who last modified the trial.
tLastModifiedTime :: Lens' Trial (Maybe UTCTime)
tLastModifiedTime = lens _tLastModifiedTime (\s a -> s {_tLastModifiedTime = a}) . mapping _Time

-- | The name of the experiment the trial is part of.
tExperimentName :: Lens' Trial (Maybe Text)
tExperimentName = lens _tExperimentName (\s a -> s {_tExperimentName = a})

-- | Undocumented member.
tSource :: Lens' Trial (Maybe TrialSource)
tSource = lens _tSource (\s a -> s {_tSource = a})

-- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
tDisplayName :: Lens' Trial (Maybe Text)
tDisplayName = lens _tDisplayName (\s a -> s {_tDisplayName = a})

-- | The name of the trial.
tTrialName :: Lens' Trial (Maybe Text)
tTrialName = lens _tTrialName (\s a -> s {_tTrialName = a})

-- | Undocumented member.
tLastModifiedBy :: Lens' Trial (Maybe UserContext)
tLastModifiedBy = lens _tLastModifiedBy (\s a -> s {_tLastModifiedBy = a})

-- | The list of tags that are associated with the trial. You can use 'Search' API to search on the tags.
tTags :: Lens' Trial [Tag]
tTags = lens _tTags (\s a -> s {_tTags = a}) . _Default . _Coerce

instance FromJSON Trial where
  parseJSON =
    withObject
      "Trial"
      ( \x ->
          Trial'
            <$> (x .:? "CreationTime")
            <*> (x .:? "TrialComponentSummaries" .!= mempty)
            <*> (x .:? "TrialArn")
            <*> (x .:? "CreatedBy")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "ExperimentName")
            <*> (x .:? "Source")
            <*> (x .:? "DisplayName")
            <*> (x .:? "TrialName")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable Trial

instance NFData Trial

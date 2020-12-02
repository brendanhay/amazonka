{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponent where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.Parent
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TrialComponentArtifact
import Network.AWS.SageMaker.Types.TrialComponentMetricSummary
import Network.AWS.SageMaker.Types.TrialComponentParameterValue
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.TrialComponentSourceDetail
import Network.AWS.SageMaker.Types.TrialComponentStatus
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of a trial component as returned by the 'Search' API.
--
--
--
-- /See:/ 'trialComponent' smart constructor.
data TrialComponent = TrialComponent'
  { _tcCreationTime ::
      !(Maybe POSIX),
    _tcStatus :: !(Maybe TrialComponentStatus),
    _tcSourceDetail :: !(Maybe TrialComponentSourceDetail),
    _tcMetrics :: !(Maybe [TrialComponentMetricSummary]),
    _tcOutputArtifacts ::
      !(Maybe (Map Text (TrialComponentArtifact))),
    _tcStartTime :: !(Maybe POSIX),
    _tcCreatedBy :: !(Maybe UserContext),
    _tcLastModifiedTime :: !(Maybe POSIX),
    _tcParents :: !(Maybe [Parent]),
    _tcEndTime :: !(Maybe POSIX),
    _tcTrialComponentName :: !(Maybe Text),
    _tcParameters ::
      !(Maybe (Map Text (TrialComponentParameterValue))),
    _tcSource :: !(Maybe TrialComponentSource),
    _tcDisplayName :: !(Maybe Text),
    _tcLastModifiedBy :: !(Maybe UserContext),
    _tcTrialComponentARN :: !(Maybe Text),
    _tcInputArtifacts ::
      !(Maybe (Map Text (TrialComponentArtifact))),
    _tcTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrialComponent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcCreationTime' - When the component was created.
--
-- * 'tcStatus' - Undocumented member.
--
-- * 'tcSourceDetail' - Details of the source of the component.
--
-- * 'tcMetrics' - The metrics for the component.
--
-- * 'tcOutputArtifacts' - The output artifacts of the component.
--
-- * 'tcStartTime' - When the component started.
--
-- * 'tcCreatedBy' - Undocumented member.
--
-- * 'tcLastModifiedTime' - When the component was last modified.
--
-- * 'tcParents' - An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
--
-- * 'tcEndTime' - When the component ended.
--
-- * 'tcTrialComponentName' - The name of the trial component.
--
-- * 'tcParameters' - The hyperparameters of the component.
--
-- * 'tcSource' - The Amazon Resource Name (ARN) and job type of the source of the component.
--
-- * 'tcDisplayName' - The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- * 'tcLastModifiedBy' - Undocumented member.
--
-- * 'tcTrialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
--
-- * 'tcInputArtifacts' - The input artifacts of the component.
--
-- * 'tcTags' - The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
trialComponent ::
  TrialComponent
trialComponent =
  TrialComponent'
    { _tcCreationTime = Nothing,
      _tcStatus = Nothing,
      _tcSourceDetail = Nothing,
      _tcMetrics = Nothing,
      _tcOutputArtifacts = Nothing,
      _tcStartTime = Nothing,
      _tcCreatedBy = Nothing,
      _tcLastModifiedTime = Nothing,
      _tcParents = Nothing,
      _tcEndTime = Nothing,
      _tcTrialComponentName = Nothing,
      _tcParameters = Nothing,
      _tcSource = Nothing,
      _tcDisplayName = Nothing,
      _tcLastModifiedBy = Nothing,
      _tcTrialComponentARN = Nothing,
      _tcInputArtifacts = Nothing,
      _tcTags = Nothing
    }

-- | When the component was created.
tcCreationTime :: Lens' TrialComponent (Maybe UTCTime)
tcCreationTime = lens _tcCreationTime (\s a -> s {_tcCreationTime = a}) . mapping _Time

-- | Undocumented member.
tcStatus :: Lens' TrialComponent (Maybe TrialComponentStatus)
tcStatus = lens _tcStatus (\s a -> s {_tcStatus = a})

-- | Details of the source of the component.
tcSourceDetail :: Lens' TrialComponent (Maybe TrialComponentSourceDetail)
tcSourceDetail = lens _tcSourceDetail (\s a -> s {_tcSourceDetail = a})

-- | The metrics for the component.
tcMetrics :: Lens' TrialComponent [TrialComponentMetricSummary]
tcMetrics = lens _tcMetrics (\s a -> s {_tcMetrics = a}) . _Default . _Coerce

-- | The output artifacts of the component.
tcOutputArtifacts :: Lens' TrialComponent (HashMap Text (TrialComponentArtifact))
tcOutputArtifacts = lens _tcOutputArtifacts (\s a -> s {_tcOutputArtifacts = a}) . _Default . _Map

-- | When the component started.
tcStartTime :: Lens' TrialComponent (Maybe UTCTime)
tcStartTime = lens _tcStartTime (\s a -> s {_tcStartTime = a}) . mapping _Time

-- | Undocumented member.
tcCreatedBy :: Lens' TrialComponent (Maybe UserContext)
tcCreatedBy = lens _tcCreatedBy (\s a -> s {_tcCreatedBy = a})

-- | When the component was last modified.
tcLastModifiedTime :: Lens' TrialComponent (Maybe UTCTime)
tcLastModifiedTime = lens _tcLastModifiedTime (\s a -> s {_tcLastModifiedTime = a}) . mapping _Time

-- | An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
tcParents :: Lens' TrialComponent [Parent]
tcParents = lens _tcParents (\s a -> s {_tcParents = a}) . _Default . _Coerce

-- | When the component ended.
tcEndTime :: Lens' TrialComponent (Maybe UTCTime)
tcEndTime = lens _tcEndTime (\s a -> s {_tcEndTime = a}) . mapping _Time

-- | The name of the trial component.
tcTrialComponentName :: Lens' TrialComponent (Maybe Text)
tcTrialComponentName = lens _tcTrialComponentName (\s a -> s {_tcTrialComponentName = a})

-- | The hyperparameters of the component.
tcParameters :: Lens' TrialComponent (HashMap Text (TrialComponentParameterValue))
tcParameters = lens _tcParameters (\s a -> s {_tcParameters = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) and job type of the source of the component.
tcSource :: Lens' TrialComponent (Maybe TrialComponentSource)
tcSource = lens _tcSource (\s a -> s {_tcSource = a})

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
tcDisplayName :: Lens' TrialComponent (Maybe Text)
tcDisplayName = lens _tcDisplayName (\s a -> s {_tcDisplayName = a})

-- | Undocumented member.
tcLastModifiedBy :: Lens' TrialComponent (Maybe UserContext)
tcLastModifiedBy = lens _tcLastModifiedBy (\s a -> s {_tcLastModifiedBy = a})

-- | The Amazon Resource Name (ARN) of the trial component.
tcTrialComponentARN :: Lens' TrialComponent (Maybe Text)
tcTrialComponentARN = lens _tcTrialComponentARN (\s a -> s {_tcTrialComponentARN = a})

-- | The input artifacts of the component.
tcInputArtifacts :: Lens' TrialComponent (HashMap Text (TrialComponentArtifact))
tcInputArtifacts = lens _tcInputArtifacts (\s a -> s {_tcInputArtifacts = a}) . _Default . _Map

-- | The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
tcTags :: Lens' TrialComponent [Tag]
tcTags = lens _tcTags (\s a -> s {_tcTags = a}) . _Default . _Coerce

instance FromJSON TrialComponent where
  parseJSON =
    withObject
      "TrialComponent"
      ( \x ->
          TrialComponent'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "SourceDetail")
            <*> (x .:? "Metrics" .!= mempty)
            <*> (x .:? "OutputArtifacts" .!= mempty)
            <*> (x .:? "StartTime")
            <*> (x .:? "CreatedBy")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "Parents" .!= mempty)
            <*> (x .:? "EndTime")
            <*> (x .:? "TrialComponentName")
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "Source")
            <*> (x .:? "DisplayName")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "TrialComponentArn")
            <*> (x .:? "InputArtifacts" .!= mempty)
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable TrialComponent

instance NFData TrialComponent

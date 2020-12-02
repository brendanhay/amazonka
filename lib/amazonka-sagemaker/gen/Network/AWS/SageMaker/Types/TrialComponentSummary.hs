{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.TrialComponentStatus
import Network.AWS.SageMaker.Types.UserContext

-- | A summary of the properties of a trial component. To get all the properties, call the 'DescribeTrialComponent' API and provide the @TrialComponentName@ .
--
--
--
-- /See:/ 'trialComponentSummary' smart constructor.
data TrialComponentSummary = TrialComponentSummary'
  { _tcsCreationTime ::
      !(Maybe POSIX),
    _tcsStatus :: !(Maybe TrialComponentStatus),
    _tcsStartTime :: !(Maybe POSIX),
    _tcsCreatedBy :: !(Maybe UserContext),
    _tcsLastModifiedTime :: !(Maybe POSIX),
    _tcsEndTime :: !(Maybe POSIX),
    _tcsTrialComponentName :: !(Maybe Text),
    _tcsDisplayName :: !(Maybe Text),
    _tcsLastModifiedBy :: !(Maybe UserContext),
    _tcsTrialComponentARN :: !(Maybe Text),
    _tcsTrialComponentSource ::
      !(Maybe TrialComponentSource)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrialComponentSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcsCreationTime' - When the component was created.
--
-- * 'tcsStatus' - The status of the component. States include:     * InProgress     * Completed     * Failed
--
-- * 'tcsStartTime' - When the component started.
--
-- * 'tcsCreatedBy' - Who created the component.
--
-- * 'tcsLastModifiedTime' - When the component was last modified.
--
-- * 'tcsEndTime' - When the component ended.
--
-- * 'tcsTrialComponentName' - The name of the trial component.
--
-- * 'tcsDisplayName' - The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- * 'tcsLastModifiedBy' - Who last modified the component.
--
-- * 'tcsTrialComponentARN' - The ARN of the trial component.
--
-- * 'tcsTrialComponentSource' - Undocumented member.
trialComponentSummary ::
  TrialComponentSummary
trialComponentSummary =
  TrialComponentSummary'
    { _tcsCreationTime = Nothing,
      _tcsStatus = Nothing,
      _tcsStartTime = Nothing,
      _tcsCreatedBy = Nothing,
      _tcsLastModifiedTime = Nothing,
      _tcsEndTime = Nothing,
      _tcsTrialComponentName = Nothing,
      _tcsDisplayName = Nothing,
      _tcsLastModifiedBy = Nothing,
      _tcsTrialComponentARN = Nothing,
      _tcsTrialComponentSource = Nothing
    }

-- | When the component was created.
tcsCreationTime :: Lens' TrialComponentSummary (Maybe UTCTime)
tcsCreationTime = lens _tcsCreationTime (\s a -> s {_tcsCreationTime = a}) . mapping _Time

-- | The status of the component. States include:     * InProgress     * Completed     * Failed
tcsStatus :: Lens' TrialComponentSummary (Maybe TrialComponentStatus)
tcsStatus = lens _tcsStatus (\s a -> s {_tcsStatus = a})

-- | When the component started.
tcsStartTime :: Lens' TrialComponentSummary (Maybe UTCTime)
tcsStartTime = lens _tcsStartTime (\s a -> s {_tcsStartTime = a}) . mapping _Time

-- | Who created the component.
tcsCreatedBy :: Lens' TrialComponentSummary (Maybe UserContext)
tcsCreatedBy = lens _tcsCreatedBy (\s a -> s {_tcsCreatedBy = a})

-- | When the component was last modified.
tcsLastModifiedTime :: Lens' TrialComponentSummary (Maybe UTCTime)
tcsLastModifiedTime = lens _tcsLastModifiedTime (\s a -> s {_tcsLastModifiedTime = a}) . mapping _Time

-- | When the component ended.
tcsEndTime :: Lens' TrialComponentSummary (Maybe UTCTime)
tcsEndTime = lens _tcsEndTime (\s a -> s {_tcsEndTime = a}) . mapping _Time

-- | The name of the trial component.
tcsTrialComponentName :: Lens' TrialComponentSummary (Maybe Text)
tcsTrialComponentName = lens _tcsTrialComponentName (\s a -> s {_tcsTrialComponentName = a})

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
tcsDisplayName :: Lens' TrialComponentSummary (Maybe Text)
tcsDisplayName = lens _tcsDisplayName (\s a -> s {_tcsDisplayName = a})

-- | Who last modified the component.
tcsLastModifiedBy :: Lens' TrialComponentSummary (Maybe UserContext)
tcsLastModifiedBy = lens _tcsLastModifiedBy (\s a -> s {_tcsLastModifiedBy = a})

-- | The ARN of the trial component.
tcsTrialComponentARN :: Lens' TrialComponentSummary (Maybe Text)
tcsTrialComponentARN = lens _tcsTrialComponentARN (\s a -> s {_tcsTrialComponentARN = a})

-- | Undocumented member.
tcsTrialComponentSource :: Lens' TrialComponentSummary (Maybe TrialComponentSource)
tcsTrialComponentSource = lens _tcsTrialComponentSource (\s a -> s {_tcsTrialComponentSource = a})

instance FromJSON TrialComponentSummary where
  parseJSON =
    withObject
      "TrialComponentSummary"
      ( \x ->
          TrialComponentSummary'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "StartTime")
            <*> (x .:? "CreatedBy")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "EndTime")
            <*> (x .:? "TrialComponentName")
            <*> (x .:? "DisplayName")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "TrialComponentArn")
            <*> (x .:? "TrialComponentSource")
      )

instance Hashable TrialComponentSummary

instance NFData TrialComponentSummary

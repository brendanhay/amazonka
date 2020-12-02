{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.GroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.GroupSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.InsightsConfiguration

-- | Details for a group without metadata.
--
--
--
-- /See:/ 'groupSummary' smart constructor.
data GroupSummary = GroupSummary'
  { _gsFilterExpression ::
      !(Maybe Text),
    _gsInsightsConfiguration :: !(Maybe InsightsConfiguration),
    _gsGroupARN :: !(Maybe Text),
    _gsGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsFilterExpression' - The filter expression defining the parameters to include traces.
--
-- * 'gsInsightsConfiguration' - The structure containing configurations related to insights.     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.     * The NotificationsEnabled boolean can be set to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
-- * 'gsGroupARN' - The ARN of the group generated based on the GroupName.
--
-- * 'gsGroupName' - The unique case-sensitive name of the group.
groupSummary ::
  GroupSummary
groupSummary =
  GroupSummary'
    { _gsFilterExpression = Nothing,
      _gsInsightsConfiguration = Nothing,
      _gsGroupARN = Nothing,
      _gsGroupName = Nothing
    }

-- | The filter expression defining the parameters to include traces.
gsFilterExpression :: Lens' GroupSummary (Maybe Text)
gsFilterExpression = lens _gsFilterExpression (\s a -> s {_gsFilterExpression = a})

-- | The structure containing configurations related to insights.     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.     * The NotificationsEnabled boolean can be set to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
gsInsightsConfiguration :: Lens' GroupSummary (Maybe InsightsConfiguration)
gsInsightsConfiguration = lens _gsInsightsConfiguration (\s a -> s {_gsInsightsConfiguration = a})

-- | The ARN of the group generated based on the GroupName.
gsGroupARN :: Lens' GroupSummary (Maybe Text)
gsGroupARN = lens _gsGroupARN (\s a -> s {_gsGroupARN = a})

-- | The unique case-sensitive name of the group.
gsGroupName :: Lens' GroupSummary (Maybe Text)
gsGroupName = lens _gsGroupName (\s a -> s {_gsGroupName = a})

instance FromJSON GroupSummary where
  parseJSON =
    withObject
      "GroupSummary"
      ( \x ->
          GroupSummary'
            <$> (x .:? "FilterExpression")
            <*> (x .:? "InsightsConfiguration")
            <*> (x .:? "GroupARN")
            <*> (x .:? "GroupName")
      )

instance Hashable GroupSummary

instance NFData GroupSummary

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.StackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.StackSummary where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.InstancesCount
import Network.AWS.Prelude

-- | Summarizes the number of layers, instances, and apps in a stack.
--
--
--
-- /See:/ 'stackSummary' smart constructor.
data StackSummary = StackSummary'
  { _ssARN :: !(Maybe Text),
    _ssAppsCount :: !(Maybe Int),
    _ssName :: !(Maybe Text),
    _ssStackId :: !(Maybe Text),
    _ssLayersCount :: !(Maybe Int),
    _ssInstancesCount :: !(Maybe InstancesCount)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssARN' - The stack's ARN.
--
-- * 'ssAppsCount' - The number of apps.
--
-- * 'ssName' - The stack name.
--
-- * 'ssStackId' - The stack ID.
--
-- * 'ssLayersCount' - The number of layers.
--
-- * 'ssInstancesCount' - An @InstancesCount@ object with the number of instances in each status.
stackSummary ::
  StackSummary
stackSummary =
  StackSummary'
    { _ssARN = Nothing,
      _ssAppsCount = Nothing,
      _ssName = Nothing,
      _ssStackId = Nothing,
      _ssLayersCount = Nothing,
      _ssInstancesCount = Nothing
    }

-- | The stack's ARN.
ssARN :: Lens' StackSummary (Maybe Text)
ssARN = lens _ssARN (\s a -> s {_ssARN = a})

-- | The number of apps.
ssAppsCount :: Lens' StackSummary (Maybe Int)
ssAppsCount = lens _ssAppsCount (\s a -> s {_ssAppsCount = a})

-- | The stack name.
ssName :: Lens' StackSummary (Maybe Text)
ssName = lens _ssName (\s a -> s {_ssName = a})

-- | The stack ID.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\s a -> s {_ssStackId = a})

-- | The number of layers.
ssLayersCount :: Lens' StackSummary (Maybe Int)
ssLayersCount = lens _ssLayersCount (\s a -> s {_ssLayersCount = a})

-- | An @InstancesCount@ object with the number of instances in each status.
ssInstancesCount :: Lens' StackSummary (Maybe InstancesCount)
ssInstancesCount = lens _ssInstancesCount (\s a -> s {_ssInstancesCount = a})

instance FromJSON StackSummary where
  parseJSON =
    withObject
      "StackSummary"
      ( \x ->
          StackSummary'
            <$> (x .:? "Arn")
            <*> (x .:? "AppsCount")
            <*> (x .:? "Name")
            <*> (x .:? "StackId")
            <*> (x .:? "LayersCount")
            <*> (x .:? "InstancesCount")
      )

instance Hashable StackSummary

instance NFData StackSummary

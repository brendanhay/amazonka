{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetInstances where

import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.CodeDeploy.Types.EC2TagSet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the instances to be used in the replacement environment in a blue/green deployment.
--
--
--
-- /See:/ 'targetInstances' smart constructor.
data TargetInstances = TargetInstances'
  { _tiEc2TagSet ::
      !(Maybe EC2TagSet),
    _tiTagFilters :: !(Maybe [EC2TagFilter]),
    _tiAutoScalingGroups :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiEc2TagSet' - Information about the groups of EC2 instance tags that an instance must be identified by in order for it to be included in the replacement environment for a blue/green deployment. Cannot be used in the same call as @tagFilters@ .
--
-- * 'tiTagFilters' - The tag filter key, type, and value used to identify Amazon EC2 instances in a replacement environment for a blue/green deployment. Cannot be used in the same call as @ec2TagSet@ .
--
-- * 'tiAutoScalingGroups' - The names of one or more Auto Scaling groups to identify a replacement environment for a blue/green deployment.
targetInstances ::
  TargetInstances
targetInstances =
  TargetInstances'
    { _tiEc2TagSet = Nothing,
      _tiTagFilters = Nothing,
      _tiAutoScalingGroups = Nothing
    }

-- | Information about the groups of EC2 instance tags that an instance must be identified by in order for it to be included in the replacement environment for a blue/green deployment. Cannot be used in the same call as @tagFilters@ .
tiEc2TagSet :: Lens' TargetInstances (Maybe EC2TagSet)
tiEc2TagSet = lens _tiEc2TagSet (\s a -> s {_tiEc2TagSet = a})

-- | The tag filter key, type, and value used to identify Amazon EC2 instances in a replacement environment for a blue/green deployment. Cannot be used in the same call as @ec2TagSet@ .
tiTagFilters :: Lens' TargetInstances [EC2TagFilter]
tiTagFilters = lens _tiTagFilters (\s a -> s {_tiTagFilters = a}) . _Default . _Coerce

-- | The names of one or more Auto Scaling groups to identify a replacement environment for a blue/green deployment.
tiAutoScalingGroups :: Lens' TargetInstances [Text]
tiAutoScalingGroups = lens _tiAutoScalingGroups (\s a -> s {_tiAutoScalingGroups = a}) . _Default . _Coerce

instance FromJSON TargetInstances where
  parseJSON =
    withObject
      "TargetInstances"
      ( \x ->
          TargetInstances'
            <$> (x .:? "ec2TagSet")
            <*> (x .:? "tagFilters" .!= mempty)
            <*> (x .:? "autoScalingGroups" .!= mempty)
      )

instance Hashable TargetInstances

instance NFData TargetInstances

instance ToJSON TargetInstances where
  toJSON TargetInstances' {..} =
    object
      ( catMaybes
          [ ("ec2TagSet" .=) <$> _tiEc2TagSet,
            ("tagFilters" .=) <$> _tiTagFilters,
            ("autoScalingGroups" .=) <$> _tiAutoScalingGroups
          ]
      )

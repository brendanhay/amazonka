{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.Summary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.Summary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroupsTagging.Types.TargetIdType

-- | A count of noncompliant resources.
--
--
--
-- /See:/ 'summary' smart constructor.
data Summary = Summary'
  { _sTargetId :: !(Maybe Text),
    _sLastUpdated :: !(Maybe Text),
    _sResourceType :: !(Maybe Text),
    _sNonCompliantResources :: !(Maybe Integer),
    _sTargetIdType :: !(Maybe TargetIdType),
    _sRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Summary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTargetId' - The account identifier or the root identifier of the organization. If you don't know the root ID, you can call the AWS Organizations <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots> API.
--
-- * 'sLastUpdated' - The timestamp that shows when this summary was generated in this Region.
--
-- * 'sResourceType' - The AWS resource type.
--
-- * 'sNonCompliantResources' - The count of noncompliant resources.
--
-- * 'sTargetIdType' - Whether the target is an account, an OU, or the organization root.
--
-- * 'sRegion' - The AWS Region that the summary applies to.
summary ::
  Summary
summary =
  Summary'
    { _sTargetId = Nothing,
      _sLastUpdated = Nothing,
      _sResourceType = Nothing,
      _sNonCompliantResources = Nothing,
      _sTargetIdType = Nothing,
      _sRegion = Nothing
    }

-- | The account identifier or the root identifier of the organization. If you don't know the root ID, you can call the AWS Organizations <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots> API.
sTargetId :: Lens' Summary (Maybe Text)
sTargetId = lens _sTargetId (\s a -> s {_sTargetId = a})

-- | The timestamp that shows when this summary was generated in this Region.
sLastUpdated :: Lens' Summary (Maybe Text)
sLastUpdated = lens _sLastUpdated (\s a -> s {_sLastUpdated = a})

-- | The AWS resource type.
sResourceType :: Lens' Summary (Maybe Text)
sResourceType = lens _sResourceType (\s a -> s {_sResourceType = a})

-- | The count of noncompliant resources.
sNonCompliantResources :: Lens' Summary (Maybe Integer)
sNonCompliantResources = lens _sNonCompliantResources (\s a -> s {_sNonCompliantResources = a})

-- | Whether the target is an account, an OU, or the organization root.
sTargetIdType :: Lens' Summary (Maybe TargetIdType)
sTargetIdType = lens _sTargetIdType (\s a -> s {_sTargetIdType = a})

-- | The AWS Region that the summary applies to.
sRegion :: Lens' Summary (Maybe Text)
sRegion = lens _sRegion (\s a -> s {_sRegion = a})

instance FromJSON Summary where
  parseJSON =
    withObject
      "Summary"
      ( \x ->
          Summary'
            <$> (x .:? "TargetId")
            <*> (x .:? "LastUpdated")
            <*> (x .:? "ResourceType")
            <*> (x .:? "NonCompliantResources")
            <*> (x .:? "TargetIdType")
            <*> (x .:? "Region")
      )

instance Hashable Summary

instance NFData Summary

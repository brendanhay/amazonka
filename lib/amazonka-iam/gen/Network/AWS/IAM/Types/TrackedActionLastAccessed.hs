{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.TrackedActionLastAccessed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.TrackedActionLastAccessed where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the most recent attempt to access an action within the service.
--
--
-- This data type is used as a response element in the 'GetServiceLastAccessedDetails' operation.
--
--
-- /See:/ 'trackedActionLastAccessed' smart constructor.
data TrackedActionLastAccessed = TrackedActionLastAccessed'
  { _talaLastAccessedTime ::
      !(Maybe ISO8601),
    _talaActionName :: !(Maybe Text),
    _talaLastAccessedEntity ::
      !(Maybe Text),
    _talaLastAccessedRegion ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrackedActionLastAccessed' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'talaLastAccessedTime' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the tracked service. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- * 'talaActionName' - The name of the tracked action to which access was attempted. Tracked actions are actions that report activity to IAM.
--
-- * 'talaLastAccessedEntity' - Undocumented member.
--
-- * 'talaLastAccessedRegion' - The Region from which the authenticated entity (user or role) last attempted to access the tracked action. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
trackedActionLastAccessed ::
  TrackedActionLastAccessed
trackedActionLastAccessed =
  TrackedActionLastAccessed'
    { _talaLastAccessedTime = Nothing,
      _talaActionName = Nothing,
      _talaLastAccessedEntity = Nothing,
      _talaLastAccessedRegion = Nothing
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the tracked service. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
talaLastAccessedTime :: Lens' TrackedActionLastAccessed (Maybe UTCTime)
talaLastAccessedTime = lens _talaLastAccessedTime (\s a -> s {_talaLastAccessedTime = a}) . mapping _Time

-- | The name of the tracked action to which access was attempted. Tracked actions are actions that report activity to IAM.
talaActionName :: Lens' TrackedActionLastAccessed (Maybe Text)
talaActionName = lens _talaActionName (\s a -> s {_talaActionName = a})

-- | Undocumented member.
talaLastAccessedEntity :: Lens' TrackedActionLastAccessed (Maybe Text)
talaLastAccessedEntity = lens _talaLastAccessedEntity (\s a -> s {_talaLastAccessedEntity = a})

-- | The Region from which the authenticated entity (user or role) last attempted to access the tracked action. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
talaLastAccessedRegion :: Lens' TrackedActionLastAccessed (Maybe Text)
talaLastAccessedRegion = lens _talaLastAccessedRegion (\s a -> s {_talaLastAccessedRegion = a})

instance FromXML TrackedActionLastAccessed where
  parseXML x =
    TrackedActionLastAccessed'
      <$> (x .@? "LastAccessedTime")
      <*> (x .@? "ActionName")
      <*> (x .@? "LastAccessedEntity")
      <*> (x .@? "LastAccessedRegion")

instance Hashable TrackedActionLastAccessed

instance NFData TrackedActionLastAccessed

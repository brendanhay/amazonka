{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EntityDetails where

import Network.AWS.IAM.Types.EntityInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains details about when the IAM entities (users or roles) were last used in an attempt to access the specified AWS service.
--
--
-- This data type is a response element in the 'GetServiceLastAccessedDetailsWithEntities' operation.
--
--
-- /See:/ 'entityDetails' smart constructor.
data EntityDetails = EntityDetails'
  { _edLastAuthenticated ::
      !(Maybe ISO8601),
    _edEntityInfo :: !EntityInfo
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edLastAuthenticated' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the authenticated entity last attempted to access AWS. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- * 'edEntityInfo' - The @EntityInfo@ object that contains details about the entity (user or role).
entityDetails ::
  -- | 'edEntityInfo'
  EntityInfo ->
  EntityDetails
entityDetails pEntityInfo_ =
  EntityDetails'
    { _edLastAuthenticated = Nothing,
      _edEntityInfo = pEntityInfo_
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the authenticated entity last attempted to access AWS. AWS does not report unauthenticated requests. This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
edLastAuthenticated :: Lens' EntityDetails (Maybe UTCTime)
edLastAuthenticated = lens _edLastAuthenticated (\s a -> s {_edLastAuthenticated = a}) . mapping _Time

-- | The @EntityInfo@ object that contains details about the entity (user or role).
edEntityInfo :: Lens' EntityDetails EntityInfo
edEntityInfo = lens _edEntityInfo (\s a -> s {_edEntityInfo = a})

instance FromXML EntityDetails where
  parseXML x =
    EntityDetails'
      <$> (x .@? "LastAuthenticated") <*> (x .@ "EntityInfo")

instance Hashable EntityDetails

instance NFData EntityDetails

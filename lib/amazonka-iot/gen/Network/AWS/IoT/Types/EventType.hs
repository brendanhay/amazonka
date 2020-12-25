{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.EventType
  ( EventType
      ( EventType',
        EventTypeThing,
        EventTypeThingGroup,
        EventTypeThingType,
        EventTypeThingGroupMembership,
        EventTypeThingGroupHierarchy,
        EventTypeThingTypeAssociation,
        EventTypeJob,
        EventTypeJobExecution,
        EventTypePolicy,
        EventTypeCertificate,
        EventTypeCaCertificate,
        fromEventType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EventType = EventType' {fromEventType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern EventTypeThing :: EventType
pattern EventTypeThing = EventType' "THING"

pattern EventTypeThingGroup :: EventType
pattern EventTypeThingGroup = EventType' "THING_GROUP"

pattern EventTypeThingType :: EventType
pattern EventTypeThingType = EventType' "THING_TYPE"

pattern EventTypeThingGroupMembership :: EventType
pattern EventTypeThingGroupMembership = EventType' "THING_GROUP_MEMBERSHIP"

pattern EventTypeThingGroupHierarchy :: EventType
pattern EventTypeThingGroupHierarchy = EventType' "THING_GROUP_HIERARCHY"

pattern EventTypeThingTypeAssociation :: EventType
pattern EventTypeThingTypeAssociation = EventType' "THING_TYPE_ASSOCIATION"

pattern EventTypeJob :: EventType
pattern EventTypeJob = EventType' "JOB"

pattern EventTypeJobExecution :: EventType
pattern EventTypeJobExecution = EventType' "JOB_EXECUTION"

pattern EventTypePolicy :: EventType
pattern EventTypePolicy = EventType' "POLICY"

pattern EventTypeCertificate :: EventType
pattern EventTypeCertificate = EventType' "CERTIFICATE"

pattern EventTypeCaCertificate :: EventType
pattern EventTypeCaCertificate = EventType' "CA_CERTIFICATE"

{-# COMPLETE
  EventTypeThing,
  EventTypeThingGroup,
  EventTypeThingType,
  EventTypeThingGroupMembership,
  EventTypeThingGroupHierarchy,
  EventTypeThingTypeAssociation,
  EventTypeJob,
  EventTypeJobExecution,
  EventTypePolicy,
  EventTypeCertificate,
  EventTypeCaCertificate,
  EventType'
  #-}

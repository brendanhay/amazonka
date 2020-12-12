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
        CaCertificate,
        Certificate,
        Job,
        JobExecution,
        Policy,
        Thing,
        ThingGroup,
        ThingGroupHierarchy,
        ThingGroupMembership,
        ThingType,
        ThingTypeAssociation
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventType = EventType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CaCertificate :: EventType
pattern CaCertificate = EventType' "CA_CERTIFICATE"

pattern Certificate :: EventType
pattern Certificate = EventType' "CERTIFICATE"

pattern Job :: EventType
pattern Job = EventType' "JOB"

pattern JobExecution :: EventType
pattern JobExecution = EventType' "JOB_EXECUTION"

pattern Policy :: EventType
pattern Policy = EventType' "POLICY"

pattern Thing :: EventType
pattern Thing = EventType' "THING"

pattern ThingGroup :: EventType
pattern ThingGroup = EventType' "THING_GROUP"

pattern ThingGroupHierarchy :: EventType
pattern ThingGroupHierarchy = EventType' "THING_GROUP_HIERARCHY"

pattern ThingGroupMembership :: EventType
pattern ThingGroupMembership = EventType' "THING_GROUP_MEMBERSHIP"

pattern ThingType :: EventType
pattern ThingType = EventType' "THING_TYPE"

pattern ThingTypeAssociation :: EventType
pattern ThingTypeAssociation = EventType' "THING_TYPE_ASSOCIATION"

{-# COMPLETE
  CaCertificate,
  Certificate,
  Job,
  JobExecution,
  Policy,
  Thing,
  ThingGroup,
  ThingGroupHierarchy,
  ThingGroupMembership,
  ThingType,
  ThingTypeAssociation,
  EventType'
  #-}

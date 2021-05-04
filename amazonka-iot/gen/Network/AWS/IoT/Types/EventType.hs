{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.EventType
  ( EventType
      ( ..,
        EventType_CA_CERTIFICATE,
        EventType_CERTIFICATE,
        EventType_JOB,
        EventType_JOB_EXECUTION,
        EventType_POLICY,
        EventType_THING,
        EventType_THING_GROUP,
        EventType_THING_GROUP_HIERARCHY,
        EventType_THING_GROUP_MEMBERSHIP,
        EventType_THING_TYPE,
        EventType_THING_TYPE_ASSOCIATION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EventType = EventType'
  { fromEventType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern EventType_CA_CERTIFICATE :: EventType
pattern EventType_CA_CERTIFICATE = EventType' "CA_CERTIFICATE"

pattern EventType_CERTIFICATE :: EventType
pattern EventType_CERTIFICATE = EventType' "CERTIFICATE"

pattern EventType_JOB :: EventType
pattern EventType_JOB = EventType' "JOB"

pattern EventType_JOB_EXECUTION :: EventType
pattern EventType_JOB_EXECUTION = EventType' "JOB_EXECUTION"

pattern EventType_POLICY :: EventType
pattern EventType_POLICY = EventType' "POLICY"

pattern EventType_THING :: EventType
pattern EventType_THING = EventType' "THING"

pattern EventType_THING_GROUP :: EventType
pattern EventType_THING_GROUP = EventType' "THING_GROUP"

pattern EventType_THING_GROUP_HIERARCHY :: EventType
pattern EventType_THING_GROUP_HIERARCHY = EventType' "THING_GROUP_HIERARCHY"

pattern EventType_THING_GROUP_MEMBERSHIP :: EventType
pattern EventType_THING_GROUP_MEMBERSHIP = EventType' "THING_GROUP_MEMBERSHIP"

pattern EventType_THING_TYPE :: EventType
pattern EventType_THING_TYPE = EventType' "THING_TYPE"

pattern EventType_THING_TYPE_ASSOCIATION :: EventType
pattern EventType_THING_TYPE_ASSOCIATION = EventType' "THING_TYPE_ASSOCIATION"

{-# COMPLETE
  EventType_CA_CERTIFICATE,
  EventType_CERTIFICATE,
  EventType_JOB,
  EventType_JOB_EXECUTION,
  EventType_POLICY,
  EventType_THING,
  EventType_THING_GROUP,
  EventType_THING_GROUP_HIERARCHY,
  EventType_THING_GROUP_MEMBERSHIP,
  EventType_THING_TYPE,
  EventType_THING_TYPE_ASSOCIATION,
  EventType'
  #-}

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
-- Module      : Network.AWS.Connect.Types.CurrentMetricName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricName
  ( CurrentMetricName
      ( ..,
        CurrentMetricName_AGENTS_AFTER_CONTACT_WORK,
        CurrentMetricName_AGENTS_AVAILABLE,
        CurrentMetricName_AGENTS_ERROR,
        CurrentMetricName_AGENTS_NON_PRODUCTIVE,
        CurrentMetricName_AGENTS_ONLINE,
        CurrentMetricName_AGENTS_ON_CALL,
        CurrentMetricName_AGENTS_ON_CONTACT,
        CurrentMetricName_AGENTS_STAFFED,
        CurrentMetricName_CONTACTS_IN_QUEUE,
        CurrentMetricName_CONTACTS_SCHEDULED,
        CurrentMetricName_OLDEST_CONTACT_AGE,
        CurrentMetricName_SLOTS_ACTIVE,
        CurrentMetricName_SLOTS_AVAILABLE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The current metric names.
newtype CurrentMetricName = CurrentMetricName'
  { fromCurrentMetricName ::
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

pattern CurrentMetricName_AGENTS_AFTER_CONTACT_WORK :: CurrentMetricName
pattern CurrentMetricName_AGENTS_AFTER_CONTACT_WORK = CurrentMetricName' "AGENTS_AFTER_CONTACT_WORK"

pattern CurrentMetricName_AGENTS_AVAILABLE :: CurrentMetricName
pattern CurrentMetricName_AGENTS_AVAILABLE = CurrentMetricName' "AGENTS_AVAILABLE"

pattern CurrentMetricName_AGENTS_ERROR :: CurrentMetricName
pattern CurrentMetricName_AGENTS_ERROR = CurrentMetricName' "AGENTS_ERROR"

pattern CurrentMetricName_AGENTS_NON_PRODUCTIVE :: CurrentMetricName
pattern CurrentMetricName_AGENTS_NON_PRODUCTIVE = CurrentMetricName' "AGENTS_NON_PRODUCTIVE"

pattern CurrentMetricName_AGENTS_ONLINE :: CurrentMetricName
pattern CurrentMetricName_AGENTS_ONLINE = CurrentMetricName' "AGENTS_ONLINE"

pattern CurrentMetricName_AGENTS_ON_CALL :: CurrentMetricName
pattern CurrentMetricName_AGENTS_ON_CALL = CurrentMetricName' "AGENTS_ON_CALL"

pattern CurrentMetricName_AGENTS_ON_CONTACT :: CurrentMetricName
pattern CurrentMetricName_AGENTS_ON_CONTACT = CurrentMetricName' "AGENTS_ON_CONTACT"

pattern CurrentMetricName_AGENTS_STAFFED :: CurrentMetricName
pattern CurrentMetricName_AGENTS_STAFFED = CurrentMetricName' "AGENTS_STAFFED"

pattern CurrentMetricName_CONTACTS_IN_QUEUE :: CurrentMetricName
pattern CurrentMetricName_CONTACTS_IN_QUEUE = CurrentMetricName' "CONTACTS_IN_QUEUE"

pattern CurrentMetricName_CONTACTS_SCHEDULED :: CurrentMetricName
pattern CurrentMetricName_CONTACTS_SCHEDULED = CurrentMetricName' "CONTACTS_SCHEDULED"

pattern CurrentMetricName_OLDEST_CONTACT_AGE :: CurrentMetricName
pattern CurrentMetricName_OLDEST_CONTACT_AGE = CurrentMetricName' "OLDEST_CONTACT_AGE"

pattern CurrentMetricName_SLOTS_ACTIVE :: CurrentMetricName
pattern CurrentMetricName_SLOTS_ACTIVE = CurrentMetricName' "SLOTS_ACTIVE"

pattern CurrentMetricName_SLOTS_AVAILABLE :: CurrentMetricName
pattern CurrentMetricName_SLOTS_AVAILABLE = CurrentMetricName' "SLOTS_AVAILABLE"

{-# COMPLETE
  CurrentMetricName_AGENTS_AFTER_CONTACT_WORK,
  CurrentMetricName_AGENTS_AVAILABLE,
  CurrentMetricName_AGENTS_ERROR,
  CurrentMetricName_AGENTS_NON_PRODUCTIVE,
  CurrentMetricName_AGENTS_ONLINE,
  CurrentMetricName_AGENTS_ON_CALL,
  CurrentMetricName_AGENTS_ON_CONTACT,
  CurrentMetricName_AGENTS_STAFFED,
  CurrentMetricName_CONTACTS_IN_QUEUE,
  CurrentMetricName_CONTACTS_SCHEDULED,
  CurrentMetricName_OLDEST_CONTACT_AGE,
  CurrentMetricName_SLOTS_ACTIVE,
  CurrentMetricName_SLOTS_AVAILABLE,
  CurrentMetricName'
  #-}

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
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationStatus
  ( ApplicationStatus
      ( ..,
        ApplicationStatus_DELETING,
        ApplicationStatus_READY,
        ApplicationStatus_RUNNING,
        ApplicationStatus_STARTING,
        ApplicationStatus_STOPPING,
        ApplicationStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ApplicationStatus = ApplicationStatus'
  { fromApplicationStatus ::
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

pattern ApplicationStatus_DELETING :: ApplicationStatus
pattern ApplicationStatus_DELETING = ApplicationStatus' "DELETING"

pattern ApplicationStatus_READY :: ApplicationStatus
pattern ApplicationStatus_READY = ApplicationStatus' "READY"

pattern ApplicationStatus_RUNNING :: ApplicationStatus
pattern ApplicationStatus_RUNNING = ApplicationStatus' "RUNNING"

pattern ApplicationStatus_STARTING :: ApplicationStatus
pattern ApplicationStatus_STARTING = ApplicationStatus' "STARTING"

pattern ApplicationStatus_STOPPING :: ApplicationStatus
pattern ApplicationStatus_STOPPING = ApplicationStatus' "STOPPING"

pattern ApplicationStatus_UPDATING :: ApplicationStatus
pattern ApplicationStatus_UPDATING = ApplicationStatus' "UPDATING"

{-# COMPLETE
  ApplicationStatus_DELETING,
  ApplicationStatus_READY,
  ApplicationStatus_RUNNING,
  ApplicationStatus_STARTING,
  ApplicationStatus_STOPPING,
  ApplicationStatus_UPDATING,
  ApplicationStatus'
  #-}

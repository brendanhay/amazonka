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
-- Module      : Network.AWS.RDS.Types.ActivityStreamStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ActivityStreamStatus
  ( ActivityStreamStatus
      ( ..,
        ActivityStreamStatus_Started,
        ActivityStreamStatus_Starting,
        ActivityStreamStatus_Stopped,
        ActivityStreamStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ActivityStreamStatus = ActivityStreamStatus'
  { fromActivityStreamStatus ::
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

pattern ActivityStreamStatus_Started :: ActivityStreamStatus
pattern ActivityStreamStatus_Started = ActivityStreamStatus' "started"

pattern ActivityStreamStatus_Starting :: ActivityStreamStatus
pattern ActivityStreamStatus_Starting = ActivityStreamStatus' "starting"

pattern ActivityStreamStatus_Stopped :: ActivityStreamStatus
pattern ActivityStreamStatus_Stopped = ActivityStreamStatus' "stopped"

pattern ActivityStreamStatus_Stopping :: ActivityStreamStatus
pattern ActivityStreamStatus_Stopping = ActivityStreamStatus' "stopping"

{-# COMPLETE
  ActivityStreamStatus_Started,
  ActivityStreamStatus_Starting,
  ActivityStreamStatus_Stopped,
  ActivityStreamStatus_Stopping,
  ActivityStreamStatus'
  #-}

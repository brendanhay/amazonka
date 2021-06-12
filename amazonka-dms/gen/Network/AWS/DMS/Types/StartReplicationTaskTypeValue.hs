{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.StartReplicationTaskTypeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.StartReplicationTaskTypeValue
  ( StartReplicationTaskTypeValue
      ( ..,
        StartReplicationTaskTypeValue_Reload_target,
        StartReplicationTaskTypeValue_Resume_processing,
        StartReplicationTaskTypeValue_Start_replication
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StartReplicationTaskTypeValue = StartReplicationTaskTypeValue'
  { fromStartReplicationTaskTypeValue ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern StartReplicationTaskTypeValue_Reload_target :: StartReplicationTaskTypeValue
pattern StartReplicationTaskTypeValue_Reload_target = StartReplicationTaskTypeValue' "reload-target"

pattern StartReplicationTaskTypeValue_Resume_processing :: StartReplicationTaskTypeValue
pattern StartReplicationTaskTypeValue_Resume_processing = StartReplicationTaskTypeValue' "resume-processing"

pattern StartReplicationTaskTypeValue_Start_replication :: StartReplicationTaskTypeValue
pattern StartReplicationTaskTypeValue_Start_replication = StartReplicationTaskTypeValue' "start-replication"

{-# COMPLETE
  StartReplicationTaskTypeValue_Reload_target,
  StartReplicationTaskTypeValue_Resume_processing,
  StartReplicationTaskTypeValue_Start_replication,
  StartReplicationTaskTypeValue'
  #-}

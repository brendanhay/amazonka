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
-- Module      : Amazonka.DMS.Types.StartReplicationTaskTypeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.StartReplicationTaskTypeValue
  ( StartReplicationTaskTypeValue
      ( ..,
        StartReplicationTaskTypeValue_Reload_target,
        StartReplicationTaskTypeValue_Resume_processing,
        StartReplicationTaskTypeValue_Start_replication
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StartReplicationTaskTypeValue = StartReplicationTaskTypeValue'
  { fromStartReplicationTaskTypeValue ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

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
-- Module      : Amazonka.ResourceGroups.Types.GroupLifecycleEventsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.GroupLifecycleEventsStatus
  ( GroupLifecycleEventsStatus
      ( ..,
        GroupLifecycleEventsStatus_ACTIVE,
        GroupLifecycleEventsStatus_ERROR,
        GroupLifecycleEventsStatus_INACTIVE,
        GroupLifecycleEventsStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GroupLifecycleEventsStatus = GroupLifecycleEventsStatus'
  { fromGroupLifecycleEventsStatus ::
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

pattern GroupLifecycleEventsStatus_ACTIVE :: GroupLifecycleEventsStatus
pattern GroupLifecycleEventsStatus_ACTIVE = GroupLifecycleEventsStatus' "ACTIVE"

pattern GroupLifecycleEventsStatus_ERROR :: GroupLifecycleEventsStatus
pattern GroupLifecycleEventsStatus_ERROR = GroupLifecycleEventsStatus' "ERROR"

pattern GroupLifecycleEventsStatus_INACTIVE :: GroupLifecycleEventsStatus
pattern GroupLifecycleEventsStatus_INACTIVE = GroupLifecycleEventsStatus' "INACTIVE"

pattern GroupLifecycleEventsStatus_IN_PROGRESS :: GroupLifecycleEventsStatus
pattern GroupLifecycleEventsStatus_IN_PROGRESS = GroupLifecycleEventsStatus' "IN_PROGRESS"

{-# COMPLETE
  GroupLifecycleEventsStatus_ACTIVE,
  GroupLifecycleEventsStatus_ERROR,
  GroupLifecycleEventsStatus_INACTIVE,
  GroupLifecycleEventsStatus_IN_PROGRESS,
  GroupLifecycleEventsStatus'
  #-}

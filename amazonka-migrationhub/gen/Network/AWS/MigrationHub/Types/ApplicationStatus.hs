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
-- Module      : Network.AWS.MigrationHub.Types.ApplicationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ApplicationStatus
  ( ApplicationStatus
      ( ..,
        ApplicationStatus_COMPLETED,
        ApplicationStatus_IN_PROGRESS,
        ApplicationStatus_NOT_STARTED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ApplicationStatus = ApplicationStatus'
  { fromApplicationStatus ::
      Core.Text
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

pattern ApplicationStatus_COMPLETED :: ApplicationStatus
pattern ApplicationStatus_COMPLETED = ApplicationStatus' "COMPLETED"

pattern ApplicationStatus_IN_PROGRESS :: ApplicationStatus
pattern ApplicationStatus_IN_PROGRESS = ApplicationStatus' "IN_PROGRESS"

pattern ApplicationStatus_NOT_STARTED :: ApplicationStatus
pattern ApplicationStatus_NOT_STARTED = ApplicationStatus' "NOT_STARTED"

{-# COMPLETE
  ApplicationStatus_COMPLETED,
  ApplicationStatus_IN_PROGRESS,
  ApplicationStatus_NOT_STARTED,
  ApplicationStatus'
  #-}

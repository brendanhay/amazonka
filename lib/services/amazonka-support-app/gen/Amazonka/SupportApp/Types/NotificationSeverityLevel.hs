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
-- Module      : Amazonka.SupportApp.Types.NotificationSeverityLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SupportApp.Types.NotificationSeverityLevel
  ( NotificationSeverityLevel
      ( ..,
        NotificationSeverityLevel_All,
        NotificationSeverityLevel_High,
        NotificationSeverityLevel_None
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype NotificationSeverityLevel = NotificationSeverityLevel'
  { fromNotificationSeverityLevel ::
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

pattern NotificationSeverityLevel_All :: NotificationSeverityLevel
pattern NotificationSeverityLevel_All = NotificationSeverityLevel' "all"

pattern NotificationSeverityLevel_High :: NotificationSeverityLevel
pattern NotificationSeverityLevel_High = NotificationSeverityLevel' "high"

pattern NotificationSeverityLevel_None :: NotificationSeverityLevel
pattern NotificationSeverityLevel_None = NotificationSeverityLevel' "none"

{-# COMPLETE
  NotificationSeverityLevel_All,
  NotificationSeverityLevel_High,
  NotificationSeverityLevel_None,
  NotificationSeverityLevel'
  #-}

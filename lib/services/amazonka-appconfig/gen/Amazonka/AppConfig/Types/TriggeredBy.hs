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
-- Module      : Amazonka.AppConfig.Types.TriggeredBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.TriggeredBy
  ( TriggeredBy
      ( ..,
        TriggeredBy_APPCONFIG,
        TriggeredBy_CLOUDWATCH_ALARM,
        TriggeredBy_INTERNAL_ERROR,
        TriggeredBy_USER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TriggeredBy = TriggeredBy'
  { fromTriggeredBy ::
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

pattern TriggeredBy_APPCONFIG :: TriggeredBy
pattern TriggeredBy_APPCONFIG = TriggeredBy' "APPCONFIG"

pattern TriggeredBy_CLOUDWATCH_ALARM :: TriggeredBy
pattern TriggeredBy_CLOUDWATCH_ALARM = TriggeredBy' "CLOUDWATCH_ALARM"

pattern TriggeredBy_INTERNAL_ERROR :: TriggeredBy
pattern TriggeredBy_INTERNAL_ERROR = TriggeredBy' "INTERNAL_ERROR"

pattern TriggeredBy_USER :: TriggeredBy
pattern TriggeredBy_USER = TriggeredBy' "USER"

{-# COMPLETE
  TriggeredBy_APPCONFIG,
  TriggeredBy_CLOUDWATCH_ALARM,
  TriggeredBy_INTERNAL_ERROR,
  TriggeredBy_USER,
  TriggeredBy'
  #-}

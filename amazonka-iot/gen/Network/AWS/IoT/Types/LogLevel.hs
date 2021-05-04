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
-- Module      : Network.AWS.IoT.Types.LogLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogLevel
  ( LogLevel
      ( ..,
        LogLevel_DEBUG,
        LogLevel_DISABLED,
        LogLevel_ERROR,
        LogLevel_INFO,
        LogLevel_WARN
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LogLevel = LogLevel'
  { fromLogLevel ::
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

pattern LogLevel_DEBUG :: LogLevel
pattern LogLevel_DEBUG = LogLevel' "DEBUG"

pattern LogLevel_DISABLED :: LogLevel
pattern LogLevel_DISABLED = LogLevel' "DISABLED"

pattern LogLevel_ERROR :: LogLevel
pattern LogLevel_ERROR = LogLevel' "ERROR"

pattern LogLevel_INFO :: LogLevel
pattern LogLevel_INFO = LogLevel' "INFO"

pattern LogLevel_WARN :: LogLevel
pattern LogLevel_WARN = LogLevel' "WARN"

{-# COMPLETE
  LogLevel_DEBUG,
  LogLevel_DISABLED,
  LogLevel_ERROR,
  LogLevel_INFO,
  LogLevel_WARN,
  LogLevel'
  #-}

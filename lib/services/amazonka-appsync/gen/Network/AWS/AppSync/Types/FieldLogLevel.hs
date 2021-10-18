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
-- Module      : Network.AWS.AppSync.Types.FieldLogLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.FieldLogLevel
  ( FieldLogLevel
      ( ..,
        FieldLogLevel_ALL,
        FieldLogLevel_ERROR,
        FieldLogLevel_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FieldLogLevel = FieldLogLevel'
  { fromFieldLogLevel ::
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

pattern FieldLogLevel_ALL :: FieldLogLevel
pattern FieldLogLevel_ALL = FieldLogLevel' "ALL"

pattern FieldLogLevel_ERROR :: FieldLogLevel
pattern FieldLogLevel_ERROR = FieldLogLevel' "ERROR"

pattern FieldLogLevel_NONE :: FieldLogLevel
pattern FieldLogLevel_NONE = FieldLogLevel' "NONE"

{-# COMPLETE
  FieldLogLevel_ALL,
  FieldLogLevel_ERROR,
  FieldLogLevel_NONE,
  FieldLogLevel'
  #-}

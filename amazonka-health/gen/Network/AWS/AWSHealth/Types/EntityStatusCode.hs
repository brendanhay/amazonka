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
-- Module      : Network.AWS.AWSHealth.Types.EntityStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityStatusCode
  ( EntityStatusCode
      ( ..,
        EntityStatusCode_IMPAIRED,
        EntityStatusCode_UNIMPAIRED,
        EntityStatusCode_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EntityStatusCode = EntityStatusCode'
  { fromEntityStatusCode ::
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

pattern EntityStatusCode_IMPAIRED :: EntityStatusCode
pattern EntityStatusCode_IMPAIRED = EntityStatusCode' "IMPAIRED"

pattern EntityStatusCode_UNIMPAIRED :: EntityStatusCode
pattern EntityStatusCode_UNIMPAIRED = EntityStatusCode' "UNIMPAIRED"

pattern EntityStatusCode_UNKNOWN :: EntityStatusCode
pattern EntityStatusCode_UNKNOWN = EntityStatusCode' "UNKNOWN"

{-# COMPLETE
  EntityStatusCode_IMPAIRED,
  EntityStatusCode_UNIMPAIRED,
  EntityStatusCode_UNKNOWN,
  EntityStatusCode'
  #-}

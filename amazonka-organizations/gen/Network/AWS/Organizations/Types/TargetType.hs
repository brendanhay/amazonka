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
-- Module      : Network.AWS.Organizations.Types.TargetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.TargetType
  ( TargetType
      ( ..,
        TargetType_ACCOUNT,
        TargetType_ORGANIZATIONAL_UNIT,
        TargetType_ROOT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TargetType = TargetType'
  { fromTargetType ::
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

pattern TargetType_ACCOUNT :: TargetType
pattern TargetType_ACCOUNT = TargetType' "ACCOUNT"

pattern TargetType_ORGANIZATIONAL_UNIT :: TargetType
pattern TargetType_ORGANIZATIONAL_UNIT = TargetType' "ORGANIZATIONAL_UNIT"

pattern TargetType_ROOT :: TargetType
pattern TargetType_ROOT = TargetType' "ROOT"

{-# COMPLETE
  TargetType_ACCOUNT,
  TargetType_ORGANIZATIONAL_UNIT,
  TargetType_ROOT,
  TargetType'
  #-}

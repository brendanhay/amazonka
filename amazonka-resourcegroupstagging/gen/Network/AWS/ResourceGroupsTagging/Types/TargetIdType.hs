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
-- Module      : Network.AWS.ResourceGroupsTagging.Types.TargetIdType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.TargetIdType
  ( TargetIdType
      ( ..,
        TargetIdType_ACCOUNT,
        TargetIdType_OU,
        TargetIdType_ROOT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TargetIdType = TargetIdType'
  { fromTargetIdType ::
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

pattern TargetIdType_ACCOUNT :: TargetIdType
pattern TargetIdType_ACCOUNT = TargetIdType' "ACCOUNT"

pattern TargetIdType_OU :: TargetIdType
pattern TargetIdType_OU = TargetIdType' "OU"

pattern TargetIdType_ROOT :: TargetIdType
pattern TargetIdType_ROOT = TargetIdType' "ROOT"

{-# COMPLETE
  TargetIdType_ACCOUNT,
  TargetIdType_OU,
  TargetIdType_ROOT,
  TargetIdType'
  #-}

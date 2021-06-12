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
-- Module      : Network.AWS.Organizations.Types.EffectivePolicyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EffectivePolicyType
  ( EffectivePolicyType
      ( ..,
        EffectivePolicyType_AISERVICES_OPT_OUT_POLICY,
        EffectivePolicyType_BACKUP_POLICY,
        EffectivePolicyType_TAG_POLICY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EffectivePolicyType = EffectivePolicyType'
  { fromEffectivePolicyType ::
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

pattern EffectivePolicyType_AISERVICES_OPT_OUT_POLICY :: EffectivePolicyType
pattern EffectivePolicyType_AISERVICES_OPT_OUT_POLICY = EffectivePolicyType' "AISERVICES_OPT_OUT_POLICY"

pattern EffectivePolicyType_BACKUP_POLICY :: EffectivePolicyType
pattern EffectivePolicyType_BACKUP_POLICY = EffectivePolicyType' "BACKUP_POLICY"

pattern EffectivePolicyType_TAG_POLICY :: EffectivePolicyType
pattern EffectivePolicyType_TAG_POLICY = EffectivePolicyType' "TAG_POLICY"

{-# COMPLETE
  EffectivePolicyType_AISERVICES_OPT_OUT_POLICY,
  EffectivePolicyType_BACKUP_POLICY,
  EffectivePolicyType_TAG_POLICY,
  EffectivePolicyType'
  #-}

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
-- Module      : Amazonka.Organizations.Types.EffectivePolicyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.EffectivePolicyType
  ( EffectivePolicyType
      ( ..,
        EffectivePolicyType_AISERVICES_OPT_OUT_POLICY,
        EffectivePolicyType_BACKUP_POLICY,
        EffectivePolicyType_TAG_POLICY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EffectivePolicyType = EffectivePolicyType'
  { fromEffectivePolicyType ::
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

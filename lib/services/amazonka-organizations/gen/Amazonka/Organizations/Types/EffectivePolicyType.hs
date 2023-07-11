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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EffectivePolicyType = EffectivePolicyType'
  { fromEffectivePolicyType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

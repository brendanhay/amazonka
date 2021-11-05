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
-- Module      : Amazonka.Organizations.Types.TargetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.TargetType
  ( TargetType
      ( ..,
        TargetType_ACCOUNT,
        TargetType_ORGANIZATIONAL_UNIT,
        TargetType_ROOT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TargetType = TargetType'
  { fromTargetType ::
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

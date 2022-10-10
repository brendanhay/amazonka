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
-- Module      : Amazonka.MacieV2.Types.EffectivePermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.EffectivePermission
  ( EffectivePermission
      ( ..,
        EffectivePermission_NOT_PUBLIC,
        EffectivePermission_PUBLIC,
        EffectivePermission_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EffectivePermission = EffectivePermission'
  { fromEffectivePermission ::
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

pattern EffectivePermission_NOT_PUBLIC :: EffectivePermission
pattern EffectivePermission_NOT_PUBLIC = EffectivePermission' "NOT_PUBLIC"

pattern EffectivePermission_PUBLIC :: EffectivePermission
pattern EffectivePermission_PUBLIC = EffectivePermission' "PUBLIC"

pattern EffectivePermission_UNKNOWN :: EffectivePermission
pattern EffectivePermission_UNKNOWN = EffectivePermission' "UNKNOWN"

{-# COMPLETE
  EffectivePermission_NOT_PUBLIC,
  EffectivePermission_PUBLIC,
  EffectivePermission_UNKNOWN,
  EffectivePermission'
  #-}

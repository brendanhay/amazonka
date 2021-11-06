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
-- Module      : Amazonka.Shield.Types.ProtectionGroupPattern
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProtectionGroupPattern
  ( ProtectionGroupPattern
      ( ..,
        ProtectionGroupPattern_ALL,
        ProtectionGroupPattern_ARBITRARY,
        ProtectionGroupPattern_BY_RESOURCE_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ProtectionGroupPattern = ProtectionGroupPattern'
  { fromProtectionGroupPattern ::
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

pattern ProtectionGroupPattern_ALL :: ProtectionGroupPattern
pattern ProtectionGroupPattern_ALL = ProtectionGroupPattern' "ALL"

pattern ProtectionGroupPattern_ARBITRARY :: ProtectionGroupPattern
pattern ProtectionGroupPattern_ARBITRARY = ProtectionGroupPattern' "ARBITRARY"

pattern ProtectionGroupPattern_BY_RESOURCE_TYPE :: ProtectionGroupPattern
pattern ProtectionGroupPattern_BY_RESOURCE_TYPE = ProtectionGroupPattern' "BY_RESOURCE_TYPE"

{-# COMPLETE
  ProtectionGroupPattern_ALL,
  ProtectionGroupPattern_ARBITRARY,
  ProtectionGroupPattern_BY_RESOURCE_TYPE,
  ProtectionGroupPattern'
  #-}

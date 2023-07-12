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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProtectionGroupPattern = ProtectionGroupPattern'
  { fromProtectionGroupPattern ::
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

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
-- Module      : Amazonka.Account.Types.AlternateContactType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Types.AlternateContactType
  ( AlternateContactType
      ( ..,
        AlternateContactType_BILLING,
        AlternateContactType_OPERATIONS,
        AlternateContactType_SECURITY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AlternateContactType = AlternateContactType'
  { fromAlternateContactType ::
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

pattern AlternateContactType_BILLING :: AlternateContactType
pattern AlternateContactType_BILLING = AlternateContactType' "BILLING"

pattern AlternateContactType_OPERATIONS :: AlternateContactType
pattern AlternateContactType_OPERATIONS = AlternateContactType' "OPERATIONS"

pattern AlternateContactType_SECURITY :: AlternateContactType
pattern AlternateContactType_SECURITY = AlternateContactType' "SECURITY"

{-# COMPLETE
  AlternateContactType_BILLING,
  AlternateContactType_OPERATIONS,
  AlternateContactType_SECURITY,
  AlternateContactType'
  #-}

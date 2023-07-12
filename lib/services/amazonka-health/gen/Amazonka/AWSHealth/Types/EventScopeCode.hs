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
-- Module      : Amazonka.AWSHealth.Types.EventScopeCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EventScopeCode
  ( EventScopeCode
      ( ..,
        EventScopeCode_ACCOUNT_SPECIFIC,
        EventScopeCode_NONE,
        EventScopeCode_PUBLIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventScopeCode = EventScopeCode'
  { fromEventScopeCode ::
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

pattern EventScopeCode_ACCOUNT_SPECIFIC :: EventScopeCode
pattern EventScopeCode_ACCOUNT_SPECIFIC = EventScopeCode' "ACCOUNT_SPECIFIC"

pattern EventScopeCode_NONE :: EventScopeCode
pattern EventScopeCode_NONE = EventScopeCode' "NONE"

pattern EventScopeCode_PUBLIC :: EventScopeCode
pattern EventScopeCode_PUBLIC = EventScopeCode' "PUBLIC"

{-# COMPLETE
  EventScopeCode_ACCOUNT_SPECIFIC,
  EventScopeCode_NONE,
  EventScopeCode_PUBLIC,
  EventScopeCode'
  #-}

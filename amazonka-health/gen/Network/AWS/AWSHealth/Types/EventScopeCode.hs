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
-- Module      : Network.AWS.AWSHealth.Types.EventScopeCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventScopeCode
  ( EventScopeCode
      ( ..,
        EventScopeCode_ACCOUNT_SPECIFIC,
        EventScopeCode_NONE,
        EventScopeCode_PUBLIC
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EventScopeCode = EventScopeCode'
  { fromEventScopeCode ::
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

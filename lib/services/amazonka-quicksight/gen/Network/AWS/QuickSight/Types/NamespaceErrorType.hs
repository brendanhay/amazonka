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
-- Module      : Amazonka.QuickSight.Types.NamespaceErrorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamespaceErrorType
  ( NamespaceErrorType
      ( ..,
        NamespaceErrorType_INTERNAL_SERVICE_ERROR,
        NamespaceErrorType_PERMISSION_DENIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype NamespaceErrorType = NamespaceErrorType'
  { fromNamespaceErrorType ::
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

pattern NamespaceErrorType_INTERNAL_SERVICE_ERROR :: NamespaceErrorType
pattern NamespaceErrorType_INTERNAL_SERVICE_ERROR = NamespaceErrorType' "INTERNAL_SERVICE_ERROR"

pattern NamespaceErrorType_PERMISSION_DENIED :: NamespaceErrorType
pattern NamespaceErrorType_PERMISSION_DENIED = NamespaceErrorType' "PERMISSION_DENIED"

{-# COMPLETE
  NamespaceErrorType_INTERNAL_SERVICE_ERROR,
  NamespaceErrorType_PERMISSION_DENIED,
  NamespaceErrorType'
  #-}

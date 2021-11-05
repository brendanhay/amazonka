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
-- Module      : Amazonka.Inspector.Types.ScopeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.ScopeType
  ( ScopeType
      ( ..,
        ScopeType_INSTANCE_ID,
        ScopeType_RULES_PACKAGE_ARN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ScopeType = ScopeType'
  { fromScopeType ::
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

pattern ScopeType_INSTANCE_ID :: ScopeType
pattern ScopeType_INSTANCE_ID = ScopeType' "INSTANCE_ID"

pattern ScopeType_RULES_PACKAGE_ARN :: ScopeType
pattern ScopeType_RULES_PACKAGE_ARN = ScopeType' "RULES_PACKAGE_ARN"

{-# COMPLETE
  ScopeType_INSTANCE_ID,
  ScopeType_RULES_PACKAGE_ARN,
  ScopeType'
  #-}

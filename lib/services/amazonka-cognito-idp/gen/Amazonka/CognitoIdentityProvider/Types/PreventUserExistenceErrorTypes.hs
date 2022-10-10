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
-- Module      : Amazonka.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
  ( PreventUserExistenceErrorTypes
      ( ..,
        PreventUserExistenceErrorTypes_ENABLED,
        PreventUserExistenceErrorTypes_LEGACY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PreventUserExistenceErrorTypes = PreventUserExistenceErrorTypes'
  { fromPreventUserExistenceErrorTypes ::
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

pattern PreventUserExistenceErrorTypes_ENABLED :: PreventUserExistenceErrorTypes
pattern PreventUserExistenceErrorTypes_ENABLED = PreventUserExistenceErrorTypes' "ENABLED"

pattern PreventUserExistenceErrorTypes_LEGACY :: PreventUserExistenceErrorTypes
pattern PreventUserExistenceErrorTypes_LEGACY = PreventUserExistenceErrorTypes' "LEGACY"

{-# COMPLETE
  PreventUserExistenceErrorTypes_ENABLED,
  PreventUserExistenceErrorTypes_LEGACY,
  PreventUserExistenceErrorTypes'
  #-}

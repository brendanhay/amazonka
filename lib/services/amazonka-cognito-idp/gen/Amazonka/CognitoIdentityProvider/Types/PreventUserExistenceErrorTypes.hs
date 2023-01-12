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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PreventUserExistenceErrorTypes = PreventUserExistenceErrorTypes'
  { fromPreventUserExistenceErrorTypes ::
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

pattern PreventUserExistenceErrorTypes_ENABLED :: PreventUserExistenceErrorTypes
pattern PreventUserExistenceErrorTypes_ENABLED = PreventUserExistenceErrorTypes' "ENABLED"

pattern PreventUserExistenceErrorTypes_LEGACY :: PreventUserExistenceErrorTypes
pattern PreventUserExistenceErrorTypes_LEGACY = PreventUserExistenceErrorTypes' "LEGACY"

{-# COMPLETE
  PreventUserExistenceErrorTypes_ENABLED,
  PreventUserExistenceErrorTypes_LEGACY,
  PreventUserExistenceErrorTypes'
  #-}

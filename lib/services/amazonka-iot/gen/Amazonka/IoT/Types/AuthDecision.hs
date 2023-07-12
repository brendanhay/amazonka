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
-- Module      : Amazonka.IoT.Types.AuthDecision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuthDecision
  ( AuthDecision
      ( ..,
        AuthDecision_ALLOWED,
        AuthDecision_EXPLICIT_DENY,
        AuthDecision_IMPLICIT_DENY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthDecision = AuthDecision'
  { fromAuthDecision ::
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

pattern AuthDecision_ALLOWED :: AuthDecision
pattern AuthDecision_ALLOWED = AuthDecision' "ALLOWED"

pattern AuthDecision_EXPLICIT_DENY :: AuthDecision
pattern AuthDecision_EXPLICIT_DENY = AuthDecision' "EXPLICIT_DENY"

pattern AuthDecision_IMPLICIT_DENY :: AuthDecision
pattern AuthDecision_IMPLICIT_DENY = AuthDecision' "IMPLICIT_DENY"

{-# COMPLETE
  AuthDecision_ALLOWED,
  AuthDecision_EXPLICIT_DENY,
  AuthDecision_IMPLICIT_DENY,
  AuthDecision'
  #-}

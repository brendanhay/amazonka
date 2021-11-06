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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype AuthDecision = AuthDecision'
  { fromAuthDecision ::
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

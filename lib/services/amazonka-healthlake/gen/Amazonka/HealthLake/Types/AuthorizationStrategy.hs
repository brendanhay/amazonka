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
-- Module      : Amazonka.HealthLake.Types.AuthorizationStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.AuthorizationStrategy
  ( AuthorizationStrategy
      ( ..,
        AuthorizationStrategy_AWS_AUTH,
        AuthorizationStrategy_SMART_ON_FHIR_V1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthorizationStrategy = AuthorizationStrategy'
  { fromAuthorizationStrategy ::
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

pattern AuthorizationStrategy_AWS_AUTH :: AuthorizationStrategy
pattern AuthorizationStrategy_AWS_AUTH = AuthorizationStrategy' "AWS_AUTH"

pattern AuthorizationStrategy_SMART_ON_FHIR_V1 :: AuthorizationStrategy
pattern AuthorizationStrategy_SMART_ON_FHIR_V1 = AuthorizationStrategy' "SMART_ON_FHIR_V1"

{-# COMPLETE
  AuthorizationStrategy_AWS_AUTH,
  AuthorizationStrategy_SMART_ON_FHIR_V1,
  AuthorizationStrategy'
  #-}

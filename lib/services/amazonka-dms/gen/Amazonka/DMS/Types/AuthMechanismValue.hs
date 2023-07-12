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
-- Module      : Amazonka.DMS.Types.AuthMechanismValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.AuthMechanismValue
  ( AuthMechanismValue
      ( ..,
        AuthMechanismValue_Default,
        AuthMechanismValue_Mongodb_cr,
        AuthMechanismValue_Scram_sha_1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthMechanismValue = AuthMechanismValue'
  { fromAuthMechanismValue ::
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

pattern AuthMechanismValue_Default :: AuthMechanismValue
pattern AuthMechanismValue_Default = AuthMechanismValue' "default"

pattern AuthMechanismValue_Mongodb_cr :: AuthMechanismValue
pattern AuthMechanismValue_Mongodb_cr = AuthMechanismValue' "mongodb_cr"

pattern AuthMechanismValue_Scram_sha_1 :: AuthMechanismValue
pattern AuthMechanismValue_Scram_sha_1 = AuthMechanismValue' "scram_sha_1"

{-# COMPLETE
  AuthMechanismValue_Default,
  AuthMechanismValue_Mongodb_cr,
  AuthMechanismValue_Scram_sha_1,
  AuthMechanismValue'
  #-}

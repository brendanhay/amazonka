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
-- Module      : Amazonka.SSM.Types.ParametersFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ParametersFilterKey
  ( ParametersFilterKey
      ( ..,
        ParametersFilterKey_KeyId,
        ParametersFilterKey_Name,
        ParametersFilterKey_Type
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ParametersFilterKey = ParametersFilterKey'
  { fromParametersFilterKey ::
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

pattern ParametersFilterKey_KeyId :: ParametersFilterKey
pattern ParametersFilterKey_KeyId = ParametersFilterKey' "KeyId"

pattern ParametersFilterKey_Name :: ParametersFilterKey
pattern ParametersFilterKey_Name = ParametersFilterKey' "Name"

pattern ParametersFilterKey_Type :: ParametersFilterKey
pattern ParametersFilterKey_Type = ParametersFilterKey' "Type"

{-# COMPLETE
  ParametersFilterKey_KeyId,
  ParametersFilterKey_Name,
  ParametersFilterKey_Type,
  ParametersFilterKey'
  #-}

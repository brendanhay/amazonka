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
-- Module      : Amazonka.ComprehendMedical.Types.ICD10CMEntityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ICD10CMEntityType
  ( ICD10CMEntityType
      ( ..,
        ICD10CMEntityType_DX_NAME,
        ICD10CMEntityType_TIME_EXPRESSION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ICD10CMEntityType = ICD10CMEntityType'
  { fromICD10CMEntityType ::
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

pattern ICD10CMEntityType_DX_NAME :: ICD10CMEntityType
pattern ICD10CMEntityType_DX_NAME = ICD10CMEntityType' "DX_NAME"

pattern ICD10CMEntityType_TIME_EXPRESSION :: ICD10CMEntityType
pattern ICD10CMEntityType_TIME_EXPRESSION = ICD10CMEntityType' "TIME_EXPRESSION"

{-# COMPLETE
  ICD10CMEntityType_DX_NAME,
  ICD10CMEntityType_TIME_EXPRESSION,
  ICD10CMEntityType'
  #-}

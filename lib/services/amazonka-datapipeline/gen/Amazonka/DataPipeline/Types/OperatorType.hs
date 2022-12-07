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
-- Module      : Amazonka.DataPipeline.Types.OperatorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.OperatorType
  ( OperatorType
      ( ..,
        OperatorType_BETWEEN,
        OperatorType_EQ,
        OperatorType_GE,
        OperatorType_LE,
        OperatorType_REF_EQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperatorType = OperatorType'
  { fromOperatorType ::
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

pattern OperatorType_BETWEEN :: OperatorType
pattern OperatorType_BETWEEN = OperatorType' "BETWEEN"

pattern OperatorType_EQ :: OperatorType
pattern OperatorType_EQ = OperatorType' "EQ"

pattern OperatorType_GE :: OperatorType
pattern OperatorType_GE = OperatorType' "GE"

pattern OperatorType_LE :: OperatorType
pattern OperatorType_LE = OperatorType' "LE"

pattern OperatorType_REF_EQ :: OperatorType
pattern OperatorType_REF_EQ = OperatorType' "REF_EQ"

{-# COMPLETE
  OperatorType_BETWEEN,
  OperatorType_EQ,
  OperatorType_GE,
  OperatorType_LE,
  OperatorType_REF_EQ,
  OperatorType'
  #-}

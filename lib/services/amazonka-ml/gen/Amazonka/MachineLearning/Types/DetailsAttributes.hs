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
-- Module      : Amazonka.MachineLearning.Types.DetailsAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.DetailsAttributes
  ( DetailsAttributes
      ( ..,
        DetailsAttributes_Algorithm,
        DetailsAttributes_PredictiveModelType
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the key values of @DetailsMap@:
--
-- -   @PredictiveModelType@ - Indicates the type of the @MLModel@.
--
-- -   @Algorithm@ - Indicates the algorithm that was used for the
--     @MLModel@.
newtype DetailsAttributes = DetailsAttributes'
  { fromDetailsAttributes ::
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

pattern DetailsAttributes_Algorithm :: DetailsAttributes
pattern DetailsAttributes_Algorithm = DetailsAttributes' "Algorithm"

pattern DetailsAttributes_PredictiveModelType :: DetailsAttributes
pattern DetailsAttributes_PredictiveModelType = DetailsAttributes' "PredictiveModelType"

{-# COMPLETE
  DetailsAttributes_Algorithm,
  DetailsAttributes_PredictiveModelType,
  DetailsAttributes'
  #-}

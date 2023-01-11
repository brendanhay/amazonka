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
-- Module      : Amazonka.EC2.Types.OfferingTypeValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.OfferingTypeValues
  ( OfferingTypeValues
      ( ..,
        OfferingTypeValues_All_Upfront,
        OfferingTypeValues_Heavy_Utilization,
        OfferingTypeValues_Light_Utilization,
        OfferingTypeValues_Medium_Utilization,
        OfferingTypeValues_No_Upfront,
        OfferingTypeValues_Partial_Upfront
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype OfferingTypeValues = OfferingTypeValues'
  { fromOfferingTypeValues ::
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

pattern OfferingTypeValues_All_Upfront :: OfferingTypeValues
pattern OfferingTypeValues_All_Upfront = OfferingTypeValues' "All Upfront"

pattern OfferingTypeValues_Heavy_Utilization :: OfferingTypeValues
pattern OfferingTypeValues_Heavy_Utilization = OfferingTypeValues' "Heavy Utilization"

pattern OfferingTypeValues_Light_Utilization :: OfferingTypeValues
pattern OfferingTypeValues_Light_Utilization = OfferingTypeValues' "Light Utilization"

pattern OfferingTypeValues_Medium_Utilization :: OfferingTypeValues
pattern OfferingTypeValues_Medium_Utilization = OfferingTypeValues' "Medium Utilization"

pattern OfferingTypeValues_No_Upfront :: OfferingTypeValues
pattern OfferingTypeValues_No_Upfront = OfferingTypeValues' "No Upfront"

pattern OfferingTypeValues_Partial_Upfront :: OfferingTypeValues
pattern OfferingTypeValues_Partial_Upfront = OfferingTypeValues' "Partial Upfront"

{-# COMPLETE
  OfferingTypeValues_All_Upfront,
  OfferingTypeValues_Heavy_Utilization,
  OfferingTypeValues_Light_Utilization,
  OfferingTypeValues_Medium_Utilization,
  OfferingTypeValues_No_Upfront,
  OfferingTypeValues_Partial_Upfront,
  OfferingTypeValues'
  #-}

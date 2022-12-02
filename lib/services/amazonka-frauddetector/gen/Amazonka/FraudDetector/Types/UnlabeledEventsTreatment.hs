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
-- Module      : Amazonka.FraudDetector.Types.UnlabeledEventsTreatment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.UnlabeledEventsTreatment
  ( UnlabeledEventsTreatment
      ( ..,
        UnlabeledEventsTreatment_FRAUD,
        UnlabeledEventsTreatment_IGNORE,
        UnlabeledEventsTreatment_LEGIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnlabeledEventsTreatment = UnlabeledEventsTreatment'
  { fromUnlabeledEventsTreatment ::
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

pattern UnlabeledEventsTreatment_FRAUD :: UnlabeledEventsTreatment
pattern UnlabeledEventsTreatment_FRAUD = UnlabeledEventsTreatment' "FRAUD"

pattern UnlabeledEventsTreatment_IGNORE :: UnlabeledEventsTreatment
pattern UnlabeledEventsTreatment_IGNORE = UnlabeledEventsTreatment' "IGNORE"

pattern UnlabeledEventsTreatment_LEGIT :: UnlabeledEventsTreatment
pattern UnlabeledEventsTreatment_LEGIT = UnlabeledEventsTreatment' "LEGIT"

{-# COMPLETE
  UnlabeledEventsTreatment_FRAUD,
  UnlabeledEventsTreatment_IGNORE,
  UnlabeledEventsTreatment_LEGIT,
  UnlabeledEventsTreatment'
  #-}

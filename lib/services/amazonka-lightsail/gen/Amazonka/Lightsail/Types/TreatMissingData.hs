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
-- Module      : Amazonka.Lightsail.Types.TreatMissingData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.TreatMissingData
  ( TreatMissingData
      ( ..,
        TreatMissingData_Breaching,
        TreatMissingData_Ignore,
        TreatMissingData_Missing,
        TreatMissingData_NotBreaching
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TreatMissingData = TreatMissingData'
  { fromTreatMissingData ::
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

pattern TreatMissingData_Breaching :: TreatMissingData
pattern TreatMissingData_Breaching = TreatMissingData' "breaching"

pattern TreatMissingData_Ignore :: TreatMissingData
pattern TreatMissingData_Ignore = TreatMissingData' "ignore"

pattern TreatMissingData_Missing :: TreatMissingData
pattern TreatMissingData_Missing = TreatMissingData' "missing"

pattern TreatMissingData_NotBreaching :: TreatMissingData
pattern TreatMissingData_NotBreaching = TreatMissingData' "notBreaching"

{-# COMPLETE
  TreatMissingData_Breaching,
  TreatMissingData_Ignore,
  TreatMissingData_Missing,
  TreatMissingData_NotBreaching,
  TreatMissingData'
  #-}

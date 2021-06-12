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
-- Module      : Network.AWS.Lightsail.Types.TreatMissingData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.TreatMissingData
  ( TreatMissingData
      ( ..,
        TreatMissingData_Breaching,
        TreatMissingData_Ignore,
        TreatMissingData_Missing,
        TreatMissingData_NotBreaching
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TreatMissingData = TreatMissingData'
  { fromTreatMissingData ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

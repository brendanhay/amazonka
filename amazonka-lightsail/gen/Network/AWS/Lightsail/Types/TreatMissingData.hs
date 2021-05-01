{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype TreatMissingData = TreatMissingData'
  { fromTreatMissingData ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

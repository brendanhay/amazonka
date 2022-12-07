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
-- Module      : Amazonka.WellArchitected.Types.LensType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensType
  ( LensType
      ( ..,
        LensType_AWS_OFFICIAL,
        LensType_CUSTOM_SELF,
        LensType_CUSTOM_SHARED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LensType = LensType'
  { fromLensType ::
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

pattern LensType_AWS_OFFICIAL :: LensType
pattern LensType_AWS_OFFICIAL = LensType' "AWS_OFFICIAL"

pattern LensType_CUSTOM_SELF :: LensType
pattern LensType_CUSTOM_SELF = LensType' "CUSTOM_SELF"

pattern LensType_CUSTOM_SHARED :: LensType
pattern LensType_CUSTOM_SHARED = LensType' "CUSTOM_SHARED"

{-# COMPLETE
  LensType_AWS_OFFICIAL,
  LensType_CUSTOM_SELF,
  LensType_CUSTOM_SHARED,
  LensType'
  #-}

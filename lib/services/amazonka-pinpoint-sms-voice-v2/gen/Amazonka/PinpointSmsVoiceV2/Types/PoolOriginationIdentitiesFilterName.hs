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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.PoolOriginationIdentitiesFilterName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.PoolOriginationIdentitiesFilterName
  ( PoolOriginationIdentitiesFilterName
      ( ..,
        PoolOriginationIdentitiesFilterName_Iso_country_code,
        PoolOriginationIdentitiesFilterName_Number_capability
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PoolOriginationIdentitiesFilterName = PoolOriginationIdentitiesFilterName'
  { fromPoolOriginationIdentitiesFilterName ::
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

pattern PoolOriginationIdentitiesFilterName_Iso_country_code :: PoolOriginationIdentitiesFilterName
pattern PoolOriginationIdentitiesFilterName_Iso_country_code = PoolOriginationIdentitiesFilterName' "iso-country-code"

pattern PoolOriginationIdentitiesFilterName_Number_capability :: PoolOriginationIdentitiesFilterName
pattern PoolOriginationIdentitiesFilterName_Number_capability = PoolOriginationIdentitiesFilterName' "number-capability"

{-# COMPLETE
  PoolOriginationIdentitiesFilterName_Iso_country_code,
  PoolOriginationIdentitiesFilterName_Number_capability,
  PoolOriginationIdentitiesFilterName'
  #-}

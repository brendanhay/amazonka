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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.DestinationCountryParameterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.DestinationCountryParameterKey
  ( DestinationCountryParameterKey
      ( ..,
        DestinationCountryParameterKey_IN_ENTITY_ID,
        DestinationCountryParameterKey_IN_TEMPLATE_ID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DestinationCountryParameterKey = DestinationCountryParameterKey'
  { fromDestinationCountryParameterKey ::
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

pattern DestinationCountryParameterKey_IN_ENTITY_ID :: DestinationCountryParameterKey
pattern DestinationCountryParameterKey_IN_ENTITY_ID = DestinationCountryParameterKey' "IN_ENTITY_ID"

pattern DestinationCountryParameterKey_IN_TEMPLATE_ID :: DestinationCountryParameterKey
pattern DestinationCountryParameterKey_IN_TEMPLATE_ID = DestinationCountryParameterKey' "IN_TEMPLATE_ID"

{-# COMPLETE
  DestinationCountryParameterKey_IN_ENTITY_ID,
  DestinationCountryParameterKey_IN_TEMPLATE_ID,
  DestinationCountryParameterKey'
  #-}

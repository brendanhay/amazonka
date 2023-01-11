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
-- Module      : Amazonka.CognitoIdentityProvider.Types.EventFilterType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.EventFilterType
  ( EventFilterType
      ( ..,
        EventFilterType_PASSWORD_CHANGE,
        EventFilterType_SIGN_IN,
        EventFilterType_SIGN_UP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventFilterType = EventFilterType'
  { fromEventFilterType ::
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

pattern EventFilterType_PASSWORD_CHANGE :: EventFilterType
pattern EventFilterType_PASSWORD_CHANGE = EventFilterType' "PASSWORD_CHANGE"

pattern EventFilterType_SIGN_IN :: EventFilterType
pattern EventFilterType_SIGN_IN = EventFilterType' "SIGN_IN"

pattern EventFilterType_SIGN_UP :: EventFilterType
pattern EventFilterType_SIGN_UP = EventFilterType' "SIGN_UP"

{-# COMPLETE
  EventFilterType_PASSWORD_CHANGE,
  EventFilterType_SIGN_IN,
  EventFilterType_SIGN_UP,
  EventFilterType'
  #-}

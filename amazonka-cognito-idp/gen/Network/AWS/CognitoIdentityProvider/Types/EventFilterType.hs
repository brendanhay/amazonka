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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventFilterType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EventFilterType
  ( EventFilterType
      ( ..,
        EventFilterType_PASSWORD_CHANGE,
        EventFilterType_SIGN_IN,
        EventFilterType_SIGN_UP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EventFilterType = EventFilterType'
  { fromEventFilterType ::
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

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
-- Module      : Network.AWS.Connect.Types.QuickConnectType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QuickConnectType
  ( QuickConnectType
      ( ..,
        QuickConnectType_PHONE_NUMBER,
        QuickConnectType_QUEUE,
        QuickConnectType_USER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype QuickConnectType = QuickConnectType'
  { fromQuickConnectType ::
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

pattern QuickConnectType_PHONE_NUMBER :: QuickConnectType
pattern QuickConnectType_PHONE_NUMBER = QuickConnectType' "PHONE_NUMBER"

pattern QuickConnectType_QUEUE :: QuickConnectType
pattern QuickConnectType_QUEUE = QuickConnectType' "QUEUE"

pattern QuickConnectType_USER :: QuickConnectType
pattern QuickConnectType_USER = QuickConnectType' "USER"

{-# COMPLETE
  QuickConnectType_PHONE_NUMBER,
  QuickConnectType_QUEUE,
  QuickConnectType_USER,
  QuickConnectType'
  #-}

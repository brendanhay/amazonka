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
-- Module      : Network.AWS.Firehose.Types.KeyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KeyType
  ( KeyType
      ( ..,
        KeyType_AWS_OWNED_CMK,
        KeyType_CUSTOMER_MANAGED_CMK
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype KeyType = KeyType' {fromKeyType :: Core.Text}
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

pattern KeyType_AWS_OWNED_CMK :: KeyType
pattern KeyType_AWS_OWNED_CMK = KeyType' "AWS_OWNED_CMK"

pattern KeyType_CUSTOMER_MANAGED_CMK :: KeyType
pattern KeyType_CUSTOMER_MANAGED_CMK = KeyType' "CUSTOMER_MANAGED_CMK"

{-# COMPLETE
  KeyType_AWS_OWNED_CMK,
  KeyType_CUSTOMER_MANAGED_CMK,
  KeyType'
  #-}

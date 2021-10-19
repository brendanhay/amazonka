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
-- Module      : Network.AWS.SESv2.Types.IdentityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.IdentityType
  ( IdentityType
      ( ..,
        IdentityType_DOMAIN,
        IdentityType_EMAIL_ADDRESS,
        IdentityType_MANAGED_DOMAIN
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype IdentityType = IdentityType'
  { fromIdentityType ::
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

pattern IdentityType_DOMAIN :: IdentityType
pattern IdentityType_DOMAIN = IdentityType' "DOMAIN"

pattern IdentityType_EMAIL_ADDRESS :: IdentityType
pattern IdentityType_EMAIL_ADDRESS = IdentityType' "EMAIL_ADDRESS"

pattern IdentityType_MANAGED_DOMAIN :: IdentityType
pattern IdentityType_MANAGED_DOMAIN = IdentityType' "MANAGED_DOMAIN"

{-# COMPLETE
  IdentityType_DOMAIN,
  IdentityType_EMAIL_ADDRESS,
  IdentityType_MANAGED_DOMAIN,
  IdentityType'
  #-}

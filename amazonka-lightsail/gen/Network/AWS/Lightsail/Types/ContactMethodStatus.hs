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
-- Module      : Network.AWS.Lightsail.Types.ContactMethodStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContactMethodStatus
  ( ContactMethodStatus
      ( ..,
        ContactMethodStatus_Invalid,
        ContactMethodStatus_PendingVerification,
        ContactMethodStatus_Valid
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ContactMethodStatus = ContactMethodStatus'
  { fromContactMethodStatus ::
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

pattern ContactMethodStatus_Invalid :: ContactMethodStatus
pattern ContactMethodStatus_Invalid = ContactMethodStatus' "Invalid"

pattern ContactMethodStatus_PendingVerification :: ContactMethodStatus
pattern ContactMethodStatus_PendingVerification = ContactMethodStatus' "PendingVerification"

pattern ContactMethodStatus_Valid :: ContactMethodStatus
pattern ContactMethodStatus_Valid = ContactMethodStatus' "Valid"

{-# COMPLETE
  ContactMethodStatus_Invalid,
  ContactMethodStatus_PendingVerification,
  ContactMethodStatus_Valid,
  ContactMethodStatus'
  #-}

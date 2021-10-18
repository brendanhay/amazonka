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
-- Module      : Network.AWS.Route53Domains.Types.ContactType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ContactType
  ( ContactType
      ( ..,
        ContactType_ASSOCIATION,
        ContactType_COMPANY,
        ContactType_PERSON,
        ContactType_PUBLIC_BODY,
        ContactType_RESELLER
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ContactType = ContactType'
  { fromContactType ::
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

pattern ContactType_ASSOCIATION :: ContactType
pattern ContactType_ASSOCIATION = ContactType' "ASSOCIATION"

pattern ContactType_COMPANY :: ContactType
pattern ContactType_COMPANY = ContactType' "COMPANY"

pattern ContactType_PERSON :: ContactType
pattern ContactType_PERSON = ContactType' "PERSON"

pattern ContactType_PUBLIC_BODY :: ContactType
pattern ContactType_PUBLIC_BODY = ContactType' "PUBLIC_BODY"

pattern ContactType_RESELLER :: ContactType
pattern ContactType_RESELLER = ContactType' "RESELLER"

{-# COMPLETE
  ContactType_ASSOCIATION,
  ContactType_COMPANY,
  ContactType_PERSON,
  ContactType_PUBLIC_BODY,
  ContactType_RESELLER,
  ContactType'
  #-}

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
-- Module      : Amazonka.Route53Domains.Types.ContactType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.ContactType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContactType = ContactType'
  { fromContactType ::
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

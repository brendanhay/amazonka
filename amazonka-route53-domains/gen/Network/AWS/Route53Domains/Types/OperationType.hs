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
-- Module      : Network.AWS.Route53Domains.Types.OperationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.OperationType
  ( OperationType
      ( ..,
        OperationType_ADD_DNSSEC,
        OperationType_CHANGE_DOMAIN_OWNER,
        OperationType_CHANGE_PRIVACY_PROTECTION,
        OperationType_DELETE_DOMAIN,
        OperationType_DISABLE_AUTORENEW,
        OperationType_DOMAIN_LOCK,
        OperationType_ENABLE_AUTORENEW,
        OperationType_EXPIRE_DOMAIN,
        OperationType_INTERNAL_TRANSFER_IN_DOMAIN,
        OperationType_INTERNAL_TRANSFER_OUT_DOMAIN,
        OperationType_PUSH_DOMAIN,
        OperationType_REGISTER_DOMAIN,
        OperationType_REMOVE_DNSSEC,
        OperationType_RENEW_DOMAIN,
        OperationType_TRANSFER_IN_DOMAIN,
        OperationType_TRANSFER_OUT_DOMAIN,
        OperationType_UPDATE_DOMAIN_CONTACT,
        OperationType_UPDATE_NAMESERVER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OperationType = OperationType'
  { fromOperationType ::
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

pattern OperationType_ADD_DNSSEC :: OperationType
pattern OperationType_ADD_DNSSEC = OperationType' "ADD_DNSSEC"

pattern OperationType_CHANGE_DOMAIN_OWNER :: OperationType
pattern OperationType_CHANGE_DOMAIN_OWNER = OperationType' "CHANGE_DOMAIN_OWNER"

pattern OperationType_CHANGE_PRIVACY_PROTECTION :: OperationType
pattern OperationType_CHANGE_PRIVACY_PROTECTION = OperationType' "CHANGE_PRIVACY_PROTECTION"

pattern OperationType_DELETE_DOMAIN :: OperationType
pattern OperationType_DELETE_DOMAIN = OperationType' "DELETE_DOMAIN"

pattern OperationType_DISABLE_AUTORENEW :: OperationType
pattern OperationType_DISABLE_AUTORENEW = OperationType' "DISABLE_AUTORENEW"

pattern OperationType_DOMAIN_LOCK :: OperationType
pattern OperationType_DOMAIN_LOCK = OperationType' "DOMAIN_LOCK"

pattern OperationType_ENABLE_AUTORENEW :: OperationType
pattern OperationType_ENABLE_AUTORENEW = OperationType' "ENABLE_AUTORENEW"

pattern OperationType_EXPIRE_DOMAIN :: OperationType
pattern OperationType_EXPIRE_DOMAIN = OperationType' "EXPIRE_DOMAIN"

pattern OperationType_INTERNAL_TRANSFER_IN_DOMAIN :: OperationType
pattern OperationType_INTERNAL_TRANSFER_IN_DOMAIN = OperationType' "INTERNAL_TRANSFER_IN_DOMAIN"

pattern OperationType_INTERNAL_TRANSFER_OUT_DOMAIN :: OperationType
pattern OperationType_INTERNAL_TRANSFER_OUT_DOMAIN = OperationType' "INTERNAL_TRANSFER_OUT_DOMAIN"

pattern OperationType_PUSH_DOMAIN :: OperationType
pattern OperationType_PUSH_DOMAIN = OperationType' "PUSH_DOMAIN"

pattern OperationType_REGISTER_DOMAIN :: OperationType
pattern OperationType_REGISTER_DOMAIN = OperationType' "REGISTER_DOMAIN"

pattern OperationType_REMOVE_DNSSEC :: OperationType
pattern OperationType_REMOVE_DNSSEC = OperationType' "REMOVE_DNSSEC"

pattern OperationType_RENEW_DOMAIN :: OperationType
pattern OperationType_RENEW_DOMAIN = OperationType' "RENEW_DOMAIN"

pattern OperationType_TRANSFER_IN_DOMAIN :: OperationType
pattern OperationType_TRANSFER_IN_DOMAIN = OperationType' "TRANSFER_IN_DOMAIN"

pattern OperationType_TRANSFER_OUT_DOMAIN :: OperationType
pattern OperationType_TRANSFER_OUT_DOMAIN = OperationType' "TRANSFER_OUT_DOMAIN"

pattern OperationType_UPDATE_DOMAIN_CONTACT :: OperationType
pattern OperationType_UPDATE_DOMAIN_CONTACT = OperationType' "UPDATE_DOMAIN_CONTACT"

pattern OperationType_UPDATE_NAMESERVER :: OperationType
pattern OperationType_UPDATE_NAMESERVER = OperationType' "UPDATE_NAMESERVER"

{-# COMPLETE
  OperationType_ADD_DNSSEC,
  OperationType_CHANGE_DOMAIN_OWNER,
  OperationType_CHANGE_PRIVACY_PROTECTION,
  OperationType_DELETE_DOMAIN,
  OperationType_DISABLE_AUTORENEW,
  OperationType_DOMAIN_LOCK,
  OperationType_ENABLE_AUTORENEW,
  OperationType_EXPIRE_DOMAIN,
  OperationType_INTERNAL_TRANSFER_IN_DOMAIN,
  OperationType_INTERNAL_TRANSFER_OUT_DOMAIN,
  OperationType_PUSH_DOMAIN,
  OperationType_REGISTER_DOMAIN,
  OperationType_REMOVE_DNSSEC,
  OperationType_RENEW_DOMAIN,
  OperationType_TRANSFER_IN_DOMAIN,
  OperationType_TRANSFER_OUT_DOMAIN,
  OperationType_UPDATE_DOMAIN_CONTACT,
  OperationType_UPDATE_NAMESERVER,
  OperationType'
  #-}

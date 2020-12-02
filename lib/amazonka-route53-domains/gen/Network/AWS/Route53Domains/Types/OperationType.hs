{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.OperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.OperationType where

import Network.AWS.Prelude

data OperationType
  = AddDNSsec
  | ChangeDomainOwner
  | ChangePrivacyProtection
  | DeleteDomain
  | DisableAutorenew
  | DomainLock
  | EnableAutorenew
  | ExpireDomain
  | InternalTransferInDomain
  | InternalTransferOutDomain
  | PushDomain
  | RegisterDomain
  | RemoveDNSsec
  | RenewDomain
  | TransferInDomain
  | TransferOutDomain
  | UpdateDomainContact
  | UpdateNameserver
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText OperationType where
  parser =
    takeLowerText >>= \case
      "add_dnssec" -> pure AddDNSsec
      "change_domain_owner" -> pure ChangeDomainOwner
      "change_privacy_protection" -> pure ChangePrivacyProtection
      "delete_domain" -> pure DeleteDomain
      "disable_autorenew" -> pure DisableAutorenew
      "domain_lock" -> pure DomainLock
      "enable_autorenew" -> pure EnableAutorenew
      "expire_domain" -> pure ExpireDomain
      "internal_transfer_in_domain" -> pure InternalTransferInDomain
      "internal_transfer_out_domain" -> pure InternalTransferOutDomain
      "push_domain" -> pure PushDomain
      "register_domain" -> pure RegisterDomain
      "remove_dnssec" -> pure RemoveDNSsec
      "renew_domain" -> pure RenewDomain
      "transfer_in_domain" -> pure TransferInDomain
      "transfer_out_domain" -> pure TransferOutDomain
      "update_domain_contact" -> pure UpdateDomainContact
      "update_nameserver" -> pure UpdateNameserver
      e ->
        fromTextError $
          "Failure parsing OperationType from value: '" <> e
            <> "'. Accepted values: add_dnssec, change_domain_owner, change_privacy_protection, delete_domain, disable_autorenew, domain_lock, enable_autorenew, expire_domain, internal_transfer_in_domain, internal_transfer_out_domain, push_domain, register_domain, remove_dnssec, renew_domain, transfer_in_domain, transfer_out_domain, update_domain_contact, update_nameserver"

instance ToText OperationType where
  toText = \case
    AddDNSsec -> "ADD_DNSSEC"
    ChangeDomainOwner -> "CHANGE_DOMAIN_OWNER"
    ChangePrivacyProtection -> "CHANGE_PRIVACY_PROTECTION"
    DeleteDomain -> "DELETE_DOMAIN"
    DisableAutorenew -> "DISABLE_AUTORENEW"
    DomainLock -> "DOMAIN_LOCK"
    EnableAutorenew -> "ENABLE_AUTORENEW"
    ExpireDomain -> "EXPIRE_DOMAIN"
    InternalTransferInDomain -> "INTERNAL_TRANSFER_IN_DOMAIN"
    InternalTransferOutDomain -> "INTERNAL_TRANSFER_OUT_DOMAIN"
    PushDomain -> "PUSH_DOMAIN"
    RegisterDomain -> "REGISTER_DOMAIN"
    RemoveDNSsec -> "REMOVE_DNSSEC"
    RenewDomain -> "RENEW_DOMAIN"
    TransferInDomain -> "TRANSFER_IN_DOMAIN"
    TransferOutDomain -> "TRANSFER_OUT_DOMAIN"
    UpdateDomainContact -> "UPDATE_DOMAIN_CONTACT"
    UpdateNameserver -> "UPDATE_NAMESERVER"

instance Hashable OperationType

instance NFData OperationType

instance ToByteString OperationType

instance ToQuery OperationType

instance ToHeader OperationType

instance FromJSON OperationType where
  parseJSON = parseJSONText "OperationType"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.OperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.OperationType
  ( OperationType
    ( OperationType'
    , OperationTypeRegisterDomain
    , OperationTypeDeleteDomain
    , OperationTypeTransferInDomain
    , OperationTypeUpdateDomainContact
    , OperationTypeUpdateNameserver
    , OperationTypeChangePrivacyProtection
    , OperationTypeDomainLock
    , OperationTypeEnableAutorenew
    , OperationTypeDisableAutorenew
    , OperationTypeAddDnssec
    , OperationTypeRemoveDnssec
    , OperationTypeExpireDomain
    , OperationTypeTransferOutDomain
    , OperationTypeChangeDomainOwner
    , OperationTypeRenewDomain
    , OperationTypePushDomain
    , OperationTypeInternalTransferOutDomain
    , OperationTypeInternalTransferInDomain
    , fromOperationType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OperationType = OperationType'{fromOperationType ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern OperationTypeRegisterDomain :: OperationType
pattern OperationTypeRegisterDomain = OperationType' "REGISTER_DOMAIN"

pattern OperationTypeDeleteDomain :: OperationType
pattern OperationTypeDeleteDomain = OperationType' "DELETE_DOMAIN"

pattern OperationTypeTransferInDomain :: OperationType
pattern OperationTypeTransferInDomain = OperationType' "TRANSFER_IN_DOMAIN"

pattern OperationTypeUpdateDomainContact :: OperationType
pattern OperationTypeUpdateDomainContact = OperationType' "UPDATE_DOMAIN_CONTACT"

pattern OperationTypeUpdateNameserver :: OperationType
pattern OperationTypeUpdateNameserver = OperationType' "UPDATE_NAMESERVER"

pattern OperationTypeChangePrivacyProtection :: OperationType
pattern OperationTypeChangePrivacyProtection = OperationType' "CHANGE_PRIVACY_PROTECTION"

pattern OperationTypeDomainLock :: OperationType
pattern OperationTypeDomainLock = OperationType' "DOMAIN_LOCK"

pattern OperationTypeEnableAutorenew :: OperationType
pattern OperationTypeEnableAutorenew = OperationType' "ENABLE_AUTORENEW"

pattern OperationTypeDisableAutorenew :: OperationType
pattern OperationTypeDisableAutorenew = OperationType' "DISABLE_AUTORENEW"

pattern OperationTypeAddDnssec :: OperationType
pattern OperationTypeAddDnssec = OperationType' "ADD_DNSSEC"

pattern OperationTypeRemoveDnssec :: OperationType
pattern OperationTypeRemoveDnssec = OperationType' "REMOVE_DNSSEC"

pattern OperationTypeExpireDomain :: OperationType
pattern OperationTypeExpireDomain = OperationType' "EXPIRE_DOMAIN"

pattern OperationTypeTransferOutDomain :: OperationType
pattern OperationTypeTransferOutDomain = OperationType' "TRANSFER_OUT_DOMAIN"

pattern OperationTypeChangeDomainOwner :: OperationType
pattern OperationTypeChangeDomainOwner = OperationType' "CHANGE_DOMAIN_OWNER"

pattern OperationTypeRenewDomain :: OperationType
pattern OperationTypeRenewDomain = OperationType' "RENEW_DOMAIN"

pattern OperationTypePushDomain :: OperationType
pattern OperationTypePushDomain = OperationType' "PUSH_DOMAIN"

pattern OperationTypeInternalTransferOutDomain :: OperationType
pattern OperationTypeInternalTransferOutDomain = OperationType' "INTERNAL_TRANSFER_OUT_DOMAIN"

pattern OperationTypeInternalTransferInDomain :: OperationType
pattern OperationTypeInternalTransferInDomain = OperationType' "INTERNAL_TRANSFER_IN_DOMAIN"

{-# COMPLETE 
  OperationTypeRegisterDomain,

  OperationTypeDeleteDomain,

  OperationTypeTransferInDomain,

  OperationTypeUpdateDomainContact,

  OperationTypeUpdateNameserver,

  OperationTypeChangePrivacyProtection,

  OperationTypeDomainLock,

  OperationTypeEnableAutorenew,

  OperationTypeDisableAutorenew,

  OperationTypeAddDnssec,

  OperationTypeRemoveDnssec,

  OperationTypeExpireDomain,

  OperationTypeTransferOutDomain,

  OperationTypeChangeDomainOwner,

  OperationTypeRenewDomain,

  OperationTypePushDomain,

  OperationTypeInternalTransferOutDomain,

  OperationTypeInternalTransferInDomain,
  OperationType'
  #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Sum where

import           Network.AWS.Prelude

data IdentityType
    = Domain
    | EmailAddress
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText IdentityType where
    parser = takeLowerText >>= \case
        "domain" -> pure Domain
        "emailaddress" -> pure EmailAddress
        e -> fromTextError $ "Failure parsing IdentityType from value: '" <> e
           <> "'. Accepted values: domain, emailaddress"

instance ToText IdentityType where
    toText = \case
        Domain -> "domain"
        EmailAddress -> "emailaddress"

instance Hashable IdentityType
instance ToQuery IdentityType
instance ToHeader IdentityType

data NotificationType
    = Delivery
    | Bounce
    | Complaint
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText NotificationType where
    parser = takeLowerText >>= \case
        "bounce" -> pure Bounce
        "complaint" -> pure Complaint
        "delivery" -> pure Delivery
        e -> fromTextError $ "Failure parsing NotificationType from value: '" <> e
           <> "'. Accepted values: bounce, complaint, delivery"

instance ToText NotificationType where
    toText = \case
        Bounce -> "bounce"
        Complaint -> "complaint"
        Delivery -> "delivery"

instance Hashable NotificationType
instance ToQuery NotificationType
instance ToHeader NotificationType

data VerificationStatus
    = NotStarted
    | Pending
    | Success
    | TemporaryFailure
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VerificationStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "notstarted" -> pure NotStarted
        "pending" -> pure Pending
        "success" -> pure Success
        "temporaryfailure" -> pure TemporaryFailure
        e -> fromTextError $ "Failure parsing VerificationStatus from value: '" <> e
           <> "'. Accepted values: failed, notstarted, pending, success, temporaryfailure"

instance ToText VerificationStatus where
    toText = \case
        Failed -> "failed"
        NotStarted -> "notstarted"
        Pending -> "pending"
        Success -> "success"
        TemporaryFailure -> "temporaryfailure"

instance Hashable VerificationStatus
instance ToQuery VerificationStatus
instance ToHeader VerificationStatus

instance FromXML VerificationStatus where
    parseXML = parseXMLText "VerificationStatus"

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
-- Module      : Network.AWS.SESV2.Types.MailFromDomainStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESV2.Types.MailFromDomainStatus
  ( MailFromDomainStatus
      ( ..,
        MailFromDomainStatus_FAILED,
        MailFromDomainStatus_PENDING,
        MailFromDomainStatus_SUCCESS,
        MailFromDomainStatus_TEMPORARY_FAILURE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The status of the MAIL FROM domain. This status can have the following
-- values:
--
-- -   @PENDING@ – Amazon SES hasn\'t started searching for the MX record
--     yet.
--
-- -   @SUCCESS@ – Amazon SES detected the required MX record for the MAIL
--     FROM domain.
--
-- -   @FAILED@ – Amazon SES can\'t find the required MX record, or the
--     record no longer exists.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue occurred, which prevented
--     Amazon SES from determining the status of the MAIL FROM domain.
newtype MailFromDomainStatus = MailFromDomainStatus'
  { fromMailFromDomainStatus ::
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

pattern MailFromDomainStatus_FAILED :: MailFromDomainStatus
pattern MailFromDomainStatus_FAILED = MailFromDomainStatus' "FAILED"

pattern MailFromDomainStatus_PENDING :: MailFromDomainStatus
pattern MailFromDomainStatus_PENDING = MailFromDomainStatus' "PENDING"

pattern MailFromDomainStatus_SUCCESS :: MailFromDomainStatus
pattern MailFromDomainStatus_SUCCESS = MailFromDomainStatus' "SUCCESS"

pattern MailFromDomainStatus_TEMPORARY_FAILURE :: MailFromDomainStatus
pattern MailFromDomainStatus_TEMPORARY_FAILURE = MailFromDomainStatus' "TEMPORARY_FAILURE"

{-# COMPLETE
  MailFromDomainStatus_FAILED,
  MailFromDomainStatus_PENDING,
  MailFromDomainStatus_SUCCESS,
  MailFromDomainStatus_TEMPORARY_FAILURE,
  MailFromDomainStatus'
  #-}

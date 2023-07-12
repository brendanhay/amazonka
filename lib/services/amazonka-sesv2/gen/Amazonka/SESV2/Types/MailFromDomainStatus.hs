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
-- Module      : Amazonka.SESV2.Types.MailFromDomainStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.MailFromDomainStatus
  ( MailFromDomainStatus
      ( ..,
        MailFromDomainStatus_FAILED,
        MailFromDomainStatus_PENDING,
        MailFromDomainStatus_SUCCESS,
        MailFromDomainStatus_TEMPORARY_FAILURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

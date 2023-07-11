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
-- Module      : Amazonka.SESV2.Types.SuppressionListReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.SuppressionListReason
  ( SuppressionListReason
      ( ..,
        SuppressionListReason_BOUNCE,
        SuppressionListReason_COMPLAINT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The reason that the address was added to the suppression list for your
-- account. The value can be one of the following:
--
-- -   @COMPLAINT@ – Amazon SES added an email address to the suppression
--     list for your account because a message sent to that address results
--     in a complaint.
--
-- -   @BOUNCE@ – Amazon SES added an email address to the suppression list
--     for your account because a message sent to that address results in a
--     hard bounce.
newtype SuppressionListReason = SuppressionListReason'
  { fromSuppressionListReason ::
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

pattern SuppressionListReason_BOUNCE :: SuppressionListReason
pattern SuppressionListReason_BOUNCE = SuppressionListReason' "BOUNCE"

pattern SuppressionListReason_COMPLAINT :: SuppressionListReason
pattern SuppressionListReason_COMPLAINT = SuppressionListReason' "COMPLAINT"

{-# COMPLETE
  SuppressionListReason_BOUNCE,
  SuppressionListReason_COMPLAINT,
  SuppressionListReason'
  #-}

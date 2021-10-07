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
-- Module      : Network.AWS.SESv2.Types.SuppressionListReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.SuppressionListReason
  ( SuppressionListReason
      ( ..,
        SuppressionListReason_BOUNCE,
        SuppressionListReason_COMPLAINT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

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

pattern SuppressionListReason_BOUNCE :: SuppressionListReason
pattern SuppressionListReason_BOUNCE = SuppressionListReason' "BOUNCE"

pattern SuppressionListReason_COMPLAINT :: SuppressionListReason
pattern SuppressionListReason_COMPLAINT = SuppressionListReason' "COMPLAINT"

{-# COMPLETE
  SuppressionListReason_BOUNCE,
  SuppressionListReason_COMPLAINT,
  SuppressionListReason'
  #-}

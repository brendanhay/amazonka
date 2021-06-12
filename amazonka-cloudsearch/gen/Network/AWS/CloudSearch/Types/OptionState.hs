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
-- Module      : Network.AWS.CloudSearch.Types.OptionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.OptionState
  ( OptionState
      ( ..,
        OptionState_Active,
        OptionState_FailedToValidate,
        OptionState_Processing,
        OptionState_RequiresIndexDocuments
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The state of processing a change to an option. One of:
--
-- -   RequiresIndexDocuments: The option\'s latest value will not be
--     deployed until IndexDocuments has been called and indexing is
--     complete.
-- -   Processing: The option\'s latest value is in the process of being
--     activated.
-- -   Active: The option\'s latest value is fully deployed.
-- -   FailedToValidate: The option value is not compatible with the
--     domain\'s data and cannot be used to index the data. You must either
--     modify the option value or update or remove the incompatible
--     documents.
newtype OptionState = OptionState'
  { fromOptionState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern OptionState_Active :: OptionState
pattern OptionState_Active = OptionState' "Active"

pattern OptionState_FailedToValidate :: OptionState
pattern OptionState_FailedToValidate = OptionState' "FailedToValidate"

pattern OptionState_Processing :: OptionState
pattern OptionState_Processing = OptionState' "Processing"

pattern OptionState_RequiresIndexDocuments :: OptionState
pattern OptionState_RequiresIndexDocuments = OptionState' "RequiresIndexDocuments"

{-# COMPLETE
  OptionState_Active,
  OptionState_FailedToValidate,
  OptionState_Processing,
  OptionState_RequiresIndexDocuments,
  OptionState'
  #-}

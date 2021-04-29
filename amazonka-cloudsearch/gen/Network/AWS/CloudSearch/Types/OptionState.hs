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

import qualified Network.AWS.Prelude as Prelude

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

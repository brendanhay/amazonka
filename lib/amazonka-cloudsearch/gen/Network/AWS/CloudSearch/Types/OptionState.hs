{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.OptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.OptionState
  ( OptionState
      ( OptionState',
        Active,
        FailedToValidate,
        Processing,
        RequiresIndexDocuments
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The state of processing a change to an option. One of:
--
--
--     * RequiresIndexDocuments: The option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.
--
--     * Processing: The option's latest value is in the process of being activated.
--
--     * Active: The option's latest value is fully deployed.
--
--     * FailedToValidate: The option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
newtype OptionState = OptionState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Active :: OptionState
pattern Active = OptionState' "Active"

pattern FailedToValidate :: OptionState
pattern FailedToValidate = OptionState' "FailedToValidate"

pattern Processing :: OptionState
pattern Processing = OptionState' "Processing"

pattern RequiresIndexDocuments :: OptionState
pattern RequiresIndexDocuments = OptionState' "RequiresIndexDocuments"

{-# COMPLETE
  Active,
  FailedToValidate,
  Processing,
  RequiresIndexDocuments,
  OptionState'
  #-}

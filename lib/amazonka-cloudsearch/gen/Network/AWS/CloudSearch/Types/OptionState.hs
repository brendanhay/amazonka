{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.OptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.OptionState
  ( OptionState
    ( OptionState'
    , OptionStateRequiresIndexDocuments
    , OptionStateProcessing
    , OptionStateActive
    , OptionStateFailedToValidate
    , fromOptionState
    )
  ) where

import qualified Network.AWS.Prelude as Core

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
--
newtype OptionState = OptionState'{fromOptionState :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern OptionStateRequiresIndexDocuments :: OptionState
pattern OptionStateRequiresIndexDocuments = OptionState' "RequiresIndexDocuments"

pattern OptionStateProcessing :: OptionState
pattern OptionStateProcessing = OptionState' "Processing"

pattern OptionStateActive :: OptionState
pattern OptionStateActive = OptionState' "Active"

pattern OptionStateFailedToValidate :: OptionState
pattern OptionStateFailedToValidate = OptionState' "FailedToValidate"

{-# COMPLETE 
  OptionStateRequiresIndexDocuments,

  OptionStateProcessing,

  OptionStateActive,

  OptionStateFailedToValidate,
  OptionState'
  #-}

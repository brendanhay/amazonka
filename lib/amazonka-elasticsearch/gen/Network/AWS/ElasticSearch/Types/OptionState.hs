{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OptionState
  ( OptionState
      ( OptionState',
        OptionStateRequiresIndexDocuments,
        OptionStateProcessing,
        OptionStateActive,
        fromOptionState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The state of a requested change. One of the following:
--
--
--     * Processing: The request change is still in-process.
--
--     * Active: The request change is processed and deployed to the Elasticsearch domain.
newtype OptionState = OptionState' {fromOptionState :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern OptionStateRequiresIndexDocuments :: OptionState
pattern OptionStateRequiresIndexDocuments = OptionState' "RequiresIndexDocuments"

pattern OptionStateProcessing :: OptionState
pattern OptionStateProcessing = OptionState' "Processing"

pattern OptionStateActive :: OptionState
pattern OptionStateActive = OptionState' "Active"

{-# COMPLETE
  OptionStateRequiresIndexDocuments,
  OptionStateProcessing,
  OptionStateActive,
  OptionState'
  #-}

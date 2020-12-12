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
        Active,
        Processing,
        RequiresIndexDocuments
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The state of a requested change. One of the following:
--
--
--     * Processing: The request change is still in-process.
--
--     * Active: The request change is processed and deployed to the Elasticsearch domain.
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

pattern Processing :: OptionState
pattern Processing = OptionState' "Processing"

pattern RequiresIndexDocuments :: OptionState
pattern RequiresIndexDocuments = OptionState' "RequiresIndexDocuments"

{-# COMPLETE
  Active,
  Processing,
  RequiresIndexDocuments,
  OptionState'
  #-}

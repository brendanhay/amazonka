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
-- Module      : Network.AWS.ElasticSearch.Types.OptionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OptionState
  ( OptionState
      ( ..,
        OptionState_Active,
        OptionState_Processing,
        OptionState_RequiresIndexDocuments
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The state of a requested change. One of the following:
--
-- -   Processing: The request change is still in-process.
-- -   Active: The request change is processed and deployed to the
--     Elasticsearch domain.
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

pattern OptionState_Processing :: OptionState
pattern OptionState_Processing = OptionState' "Processing"

pattern OptionState_RequiresIndexDocuments :: OptionState
pattern OptionState_RequiresIndexDocuments = OptionState' "RequiresIndexDocuments"

{-# COMPLETE
  OptionState_Active,
  OptionState_Processing,
  OptionState_RequiresIndexDocuments,
  OptionState'
  #-}

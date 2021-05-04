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

import qualified Network.AWS.Prelude as Prelude

-- | The state of a requested change. One of the following:
--
-- -   Processing: The request change is still in-process.
-- -   Active: The request change is processed and deployed to the
--     Elasticsearch domain.
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

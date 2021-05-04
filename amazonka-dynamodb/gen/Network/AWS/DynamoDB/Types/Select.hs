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
-- Module      : Network.AWS.DynamoDB.Types.Select
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Select
  ( Select
      ( ..,
        Select_ALL_ATTRIBUTES,
        Select_ALL_PROJECTED_ATTRIBUTES,
        Select_COUNT,
        Select_SPECIFIC_ATTRIBUTES
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype Select = Select' {fromSelect :: Prelude.Text}
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

pattern Select_ALL_ATTRIBUTES :: Select
pattern Select_ALL_ATTRIBUTES = Select' "ALL_ATTRIBUTES"

pattern Select_ALL_PROJECTED_ATTRIBUTES :: Select
pattern Select_ALL_PROJECTED_ATTRIBUTES = Select' "ALL_PROJECTED_ATTRIBUTES"

pattern Select_COUNT :: Select
pattern Select_COUNT = Select' "COUNT"

pattern Select_SPECIFIC_ATTRIBUTES :: Select
pattern Select_SPECIFIC_ATTRIBUTES = Select' "SPECIFIC_ATTRIBUTES"

{-# COMPLETE
  Select_ALL_ATTRIBUTES,
  Select_ALL_PROJECTED_ATTRIBUTES,
  Select_COUNT,
  Select_SPECIFIC_ATTRIBUTES,
  Select'
  #-}

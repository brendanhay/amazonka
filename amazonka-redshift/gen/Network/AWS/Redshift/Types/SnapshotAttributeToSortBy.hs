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
-- Module      : Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
  ( SnapshotAttributeToSortBy
      ( ..,
        SnapshotAttributeToSortBy_CREATE_TIME,
        SnapshotAttributeToSortBy_SOURCE_TYPE,
        SnapshotAttributeToSortBy_TOTAL_SIZE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype SnapshotAttributeToSortBy = SnapshotAttributeToSortBy'
  { fromSnapshotAttributeToSortBy ::
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

pattern SnapshotAttributeToSortBy_CREATE_TIME :: SnapshotAttributeToSortBy
pattern SnapshotAttributeToSortBy_CREATE_TIME = SnapshotAttributeToSortBy' "CREATE_TIME"

pattern SnapshotAttributeToSortBy_SOURCE_TYPE :: SnapshotAttributeToSortBy
pattern SnapshotAttributeToSortBy_SOURCE_TYPE = SnapshotAttributeToSortBy' "SOURCE_TYPE"

pattern SnapshotAttributeToSortBy_TOTAL_SIZE :: SnapshotAttributeToSortBy
pattern SnapshotAttributeToSortBy_TOTAL_SIZE = SnapshotAttributeToSortBy' "TOTAL_SIZE"

{-# COMPLETE
  SnapshotAttributeToSortBy_CREATE_TIME,
  SnapshotAttributeToSortBy_SOURCE_TYPE,
  SnapshotAttributeToSortBy_TOTAL_SIZE,
  SnapshotAttributeToSortBy'
  #-}

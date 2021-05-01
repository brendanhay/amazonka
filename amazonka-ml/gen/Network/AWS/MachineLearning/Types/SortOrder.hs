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
-- Module      : Network.AWS.MachineLearning.Types.SortOrder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.SortOrder
  ( SortOrder
      ( ..,
        SortOrder_Asc,
        SortOrder_Dsc
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The sort order specified in a listing condition. Possible values include
-- the following:
--
-- -   @asc@ - Present the information in ascending order (from A-Z).
-- -   @dsc@ - Present the information in descending order (from Z-A).
newtype SortOrder = SortOrder'
  { fromSortOrder ::
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

pattern SortOrder_Asc :: SortOrder
pattern SortOrder_Asc = SortOrder' "asc"

pattern SortOrder_Dsc :: SortOrder
pattern SortOrder_Dsc = SortOrder' "dsc"

{-# COMPLETE
  SortOrder_Asc,
  SortOrder_Dsc,
  SortOrder'
  #-}

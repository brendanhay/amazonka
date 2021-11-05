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
-- Module      : Network.AWS.MacieV2.Types.SearchResourcesComparator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.SearchResourcesComparator
  ( SearchResourcesComparator
      ( ..,
        SearchResourcesComparator_EQ,
        SearchResourcesComparator_NE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The operator to use in a condition that filters the results of a query.
-- Valid values are:
newtype SearchResourcesComparator = SearchResourcesComparator'
  { fromSearchResourcesComparator ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern SearchResourcesComparator_EQ :: SearchResourcesComparator
pattern SearchResourcesComparator_EQ = SearchResourcesComparator' "EQ"

pattern SearchResourcesComparator_NE :: SearchResourcesComparator
pattern SearchResourcesComparator_NE = SearchResourcesComparator' "NE"

{-# COMPLETE
  SearchResourcesComparator_EQ,
  SearchResourcesComparator_NE,
  SearchResourcesComparator'
  #-}

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
-- Module      : Network.AWS.CodeBuild.Types.SharedResourceSortByType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SharedResourceSortByType
  ( SharedResourceSortByType
      ( ..,
        SharedResourceSortByType_ARN,
        SharedResourceSortByType_MODIFIED_TIME
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SharedResourceSortByType = SharedResourceSortByType'
  { fromSharedResourceSortByType ::
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

pattern SharedResourceSortByType_ARN :: SharedResourceSortByType
pattern SharedResourceSortByType_ARN = SharedResourceSortByType' "ARN"

pattern SharedResourceSortByType_MODIFIED_TIME :: SharedResourceSortByType
pattern SharedResourceSortByType_MODIFIED_TIME = SharedResourceSortByType' "MODIFIED_TIME"

{-# COMPLETE
  SharedResourceSortByType_ARN,
  SharedResourceSortByType_MODIFIED_TIME,
  SharedResourceSortByType'
  #-}

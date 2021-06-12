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

import qualified Network.AWS.Core as Core

newtype SharedResourceSortByType = SharedResourceSortByType'
  { fromSharedResourceSortByType ::
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

pattern SharedResourceSortByType_ARN :: SharedResourceSortByType
pattern SharedResourceSortByType_ARN = SharedResourceSortByType' "ARN"

pattern SharedResourceSortByType_MODIFIED_TIME :: SharedResourceSortByType
pattern SharedResourceSortByType_MODIFIED_TIME = SharedResourceSortByType' "MODIFIED_TIME"

{-# COMPLETE
  SharedResourceSortByType_ARN,
  SharedResourceSortByType_MODIFIED_TIME,
  SharedResourceSortByType'
  #-}

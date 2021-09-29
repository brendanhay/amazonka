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
-- Module      : Network.AWS.CloudFormation.Types.Category
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Category
  ( Category
      ( ..,
        Category_ACTIVATED,
        Category_AWS_TYPES,
        Category_REGISTERED,
        Category_THIRD_PARTY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype Category = Category'
  { fromCategory ::
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

pattern Category_ACTIVATED :: Category
pattern Category_ACTIVATED = Category' "ACTIVATED"

pattern Category_AWS_TYPES :: Category
pattern Category_AWS_TYPES = Category' "AWS_TYPES"

pattern Category_REGISTERED :: Category
pattern Category_REGISTERED = Category' "REGISTERED"

pattern Category_THIRD_PARTY :: Category
pattern Category_THIRD_PARTY = Category' "THIRD_PARTY"

{-# COMPLETE
  Category_ACTIVATED,
  Category_AWS_TYPES,
  Category_REGISTERED,
  Category_THIRD_PARTY,
  Category'
  #-}

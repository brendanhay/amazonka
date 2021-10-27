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
-- Module      : Network.AWS.ComprehendMedical.Types.RxNormEntityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComprehendMedical.Types.RxNormEntityType
  ( RxNormEntityType
      ( ..,
        RxNormEntityType_BRAND_NAME,
        RxNormEntityType_GENERIC_NAME
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RxNormEntityType = RxNormEntityType'
  { fromRxNormEntityType ::
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

pattern RxNormEntityType_BRAND_NAME :: RxNormEntityType
pattern RxNormEntityType_BRAND_NAME = RxNormEntityType' "BRAND_NAME"

pattern RxNormEntityType_GENERIC_NAME :: RxNormEntityType
pattern RxNormEntityType_GENERIC_NAME = RxNormEntityType' "GENERIC_NAME"

{-# COMPLETE
  RxNormEntityType_BRAND_NAME,
  RxNormEntityType_GENERIC_NAME,
  RxNormEntityType'
  #-}

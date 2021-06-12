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
-- Module      : Network.AWS.CloudFront.Types.PriceClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PriceClass
  ( PriceClass
      ( ..,
        PriceClass_PriceClass_100,
        PriceClass_PriceClass_200,
        PriceClass_PriceClass_All
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PriceClass = PriceClass'
  { fromPriceClass ::
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

pattern PriceClass_PriceClass_100 :: PriceClass
pattern PriceClass_PriceClass_100 = PriceClass' "PriceClass_100"

pattern PriceClass_PriceClass_200 :: PriceClass
pattern PriceClass_PriceClass_200 = PriceClass' "PriceClass_200"

pattern PriceClass_PriceClass_All :: PriceClass
pattern PriceClass_PriceClass_All = PriceClass' "PriceClass_All"

{-# COMPLETE
  PriceClass_PriceClass_100,
  PriceClass_PriceClass_200,
  PriceClass_PriceClass_All,
  PriceClass'
  #-}

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

import qualified Network.AWS.Prelude as Prelude

newtype PriceClass = PriceClass'
  { fromPriceClass ::
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

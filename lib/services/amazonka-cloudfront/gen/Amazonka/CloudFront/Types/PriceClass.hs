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
-- Module      : Amazonka.CloudFront.Types.PriceClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.PriceClass
  ( PriceClass
      ( ..,
        PriceClass_PriceClass_100,
        PriceClass_PriceClass_200,
        PriceClass_PriceClass_All
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PriceClass = PriceClass'
  { fromPriceClass ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

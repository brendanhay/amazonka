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
-- Module      : Amazonka.QuickSight.Types.NumberScale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumberScale
  ( NumberScale
      ( ..,
        NumberScale_AUTO,
        NumberScale_BILLIONS,
        NumberScale_MILLIONS,
        NumberScale_NONE,
        NumberScale_THOUSANDS,
        NumberScale_TRILLIONS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NumberScale = NumberScale'
  { fromNumberScale ::
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

pattern NumberScale_AUTO :: NumberScale
pattern NumberScale_AUTO = NumberScale' "AUTO"

pattern NumberScale_BILLIONS :: NumberScale
pattern NumberScale_BILLIONS = NumberScale' "BILLIONS"

pattern NumberScale_MILLIONS :: NumberScale
pattern NumberScale_MILLIONS = NumberScale' "MILLIONS"

pattern NumberScale_NONE :: NumberScale
pattern NumberScale_NONE = NumberScale' "NONE"

pattern NumberScale_THOUSANDS :: NumberScale
pattern NumberScale_THOUSANDS = NumberScale' "THOUSANDS"

pattern NumberScale_TRILLIONS :: NumberScale
pattern NumberScale_TRILLIONS = NumberScale' "TRILLIONS"

{-# COMPLETE
  NumberScale_AUTO,
  NumberScale_BILLIONS,
  NumberScale_MILLIONS,
  NumberScale_NONE,
  NumberScale_THOUSANDS,
  NumberScale_TRILLIONS,
  NumberScale'
  #-}

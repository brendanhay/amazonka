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
-- Module      : Amazonka.QuickSight.Types.RelativeFontSize
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RelativeFontSize
  ( RelativeFontSize
      ( ..,
        RelativeFontSize_EXTRA_LARGE,
        RelativeFontSize_EXTRA_SMALL,
        RelativeFontSize_LARGE,
        RelativeFontSize_MEDIUM,
        RelativeFontSize_SMALL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RelativeFontSize = RelativeFontSize'
  { fromRelativeFontSize ::
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

pattern RelativeFontSize_EXTRA_LARGE :: RelativeFontSize
pattern RelativeFontSize_EXTRA_LARGE = RelativeFontSize' "EXTRA_LARGE"

pattern RelativeFontSize_EXTRA_SMALL :: RelativeFontSize
pattern RelativeFontSize_EXTRA_SMALL = RelativeFontSize' "EXTRA_SMALL"

pattern RelativeFontSize_LARGE :: RelativeFontSize
pattern RelativeFontSize_LARGE = RelativeFontSize' "LARGE"

pattern RelativeFontSize_MEDIUM :: RelativeFontSize
pattern RelativeFontSize_MEDIUM = RelativeFontSize' "MEDIUM"

pattern RelativeFontSize_SMALL :: RelativeFontSize
pattern RelativeFontSize_SMALL = RelativeFontSize' "SMALL"

{-# COMPLETE
  RelativeFontSize_EXTRA_LARGE,
  RelativeFontSize_EXTRA_SMALL,
  RelativeFontSize_LARGE,
  RelativeFontSize_MEDIUM,
  RelativeFontSize_SMALL,
  RelativeFontSize'
  #-}

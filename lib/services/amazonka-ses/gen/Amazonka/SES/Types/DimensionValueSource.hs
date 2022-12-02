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
-- Module      : Amazonka.SES.Types.DimensionValueSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.DimensionValueSource
  ( DimensionValueSource
      ( ..,
        DimensionValueSource_EmailHeader,
        DimensionValueSource_LinkTag,
        DimensionValueSource_MessageTag
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DimensionValueSource = DimensionValueSource'
  { fromDimensionValueSource ::
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

pattern DimensionValueSource_EmailHeader :: DimensionValueSource
pattern DimensionValueSource_EmailHeader = DimensionValueSource' "emailHeader"

pattern DimensionValueSource_LinkTag :: DimensionValueSource
pattern DimensionValueSource_LinkTag = DimensionValueSource' "linkTag"

pattern DimensionValueSource_MessageTag :: DimensionValueSource
pattern DimensionValueSource_MessageTag = DimensionValueSource' "messageTag"

{-# COMPLETE
  DimensionValueSource_EmailHeader,
  DimensionValueSource_LinkTag,
  DimensionValueSource_MessageTag,
  DimensionValueSource'
  #-}

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
-- Module      : Network.AWS.SES.Types.DimensionValueSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.DimensionValueSource
  ( DimensionValueSource
      ( ..,
        DimensionValueSource_EmailHeader,
        DimensionValueSource_LinkTag,
        DimensionValueSource_MessageTag
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DimensionValueSource = DimensionValueSource'
  { fromDimensionValueSource ::
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

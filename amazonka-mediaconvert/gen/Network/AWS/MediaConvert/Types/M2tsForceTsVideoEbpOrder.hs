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
-- Module      : Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
  ( M2tsForceTsVideoEbpOrder
      ( ..,
        M2tsForceTsVideoEbpOrder_DEFAULT,
        M2tsForceTsVideoEbpOrder_FORCE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Keep the default value (DEFAULT) unless you know that your audio EBP
-- markers are incorrectly appearing before your video EBP markers. To
-- correct this problem, set this value to Force (FORCE).
newtype M2tsForceTsVideoEbpOrder = M2tsForceTsVideoEbpOrder'
  { fromM2tsForceTsVideoEbpOrder ::
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

pattern M2tsForceTsVideoEbpOrder_DEFAULT :: M2tsForceTsVideoEbpOrder
pattern M2tsForceTsVideoEbpOrder_DEFAULT = M2tsForceTsVideoEbpOrder' "DEFAULT"

pattern M2tsForceTsVideoEbpOrder_FORCE :: M2tsForceTsVideoEbpOrder
pattern M2tsForceTsVideoEbpOrder_FORCE = M2tsForceTsVideoEbpOrder' "FORCE"

{-# COMPLETE
  M2tsForceTsVideoEbpOrder_DEFAULT,
  M2tsForceTsVideoEbpOrder_FORCE,
  M2tsForceTsVideoEbpOrder'
  #-}

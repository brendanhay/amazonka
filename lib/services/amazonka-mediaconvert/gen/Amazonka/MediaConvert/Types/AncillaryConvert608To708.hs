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
-- Module      : Amazonka.MediaConvert.Types.AncillaryConvert608To708
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AncillaryConvert608To708
  ( AncillaryConvert608To708
      ( ..,
        AncillaryConvert608To708_DISABLED,
        AncillaryConvert608To708_UPCONVERT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether this set of input captions appears in your outputs in
-- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
-- MediaConvert includes the captions data in two ways: it passes the 608
-- data through using the 608 compatibility bytes fields of the 708
-- wrapper, and it also translates the 608 data into 708.
newtype AncillaryConvert608To708 = AncillaryConvert608To708'
  { fromAncillaryConvert608To708 ::
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

pattern AncillaryConvert608To708_DISABLED :: AncillaryConvert608To708
pattern AncillaryConvert608To708_DISABLED = AncillaryConvert608To708' "DISABLED"

pattern AncillaryConvert608To708_UPCONVERT :: AncillaryConvert608To708
pattern AncillaryConvert608To708_UPCONVERT = AncillaryConvert608To708' "UPCONVERT"

{-# COMPLETE
  AncillaryConvert608To708_DISABLED,
  AncillaryConvert608To708_UPCONVERT,
  AncillaryConvert608To708'
  #-}

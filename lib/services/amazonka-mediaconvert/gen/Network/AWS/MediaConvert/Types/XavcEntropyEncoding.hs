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
-- Module      : Amazonka.MediaConvert.Types.XavcEntropyEncoding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcEntropyEncoding
  ( XavcEntropyEncoding
      ( ..,
        XavcEntropyEncoding_AUTO,
        XavcEntropyEncoding_CABAC,
        XavcEntropyEncoding_CAVLC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Optional. Choose a specific entropy encoding mode only when you want to
-- override XAVC recommendations. If you choose the value auto,
-- MediaConvert uses the mode that the XAVC file format specifies given
-- this output\'s operating point.
newtype XavcEntropyEncoding = XavcEntropyEncoding'
  { fromXavcEntropyEncoding ::
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

pattern XavcEntropyEncoding_AUTO :: XavcEntropyEncoding
pattern XavcEntropyEncoding_AUTO = XavcEntropyEncoding' "AUTO"

pattern XavcEntropyEncoding_CABAC :: XavcEntropyEncoding
pattern XavcEntropyEncoding_CABAC = XavcEntropyEncoding' "CABAC"

pattern XavcEntropyEncoding_CAVLC :: XavcEntropyEncoding
pattern XavcEntropyEncoding_CAVLC = XavcEntropyEncoding' "CAVLC"

{-# COMPLETE
  XavcEntropyEncoding_AUTO,
  XavcEntropyEncoding_CABAC,
  XavcEntropyEncoding_CAVLC,
  XavcEntropyEncoding'
  #-}

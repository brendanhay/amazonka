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
-- Module      : Amazonka.MediaConvert.Types.XavcGopBReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcGopBReference
  ( XavcGopBReference
      ( ..,
        XavcGopBReference_DISABLED,
        XavcGopBReference_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether the encoder uses B-frames as reference frames for other
-- pictures in the same GOP. Choose Allow (ENABLED) to allow the encoder to
-- use B-frames as reference frames. Choose Don\'t allow (DISABLED) to
-- prevent the encoder from using B-frames as reference frames.
newtype XavcGopBReference = XavcGopBReference'
  { fromXavcGopBReference ::
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

pattern XavcGopBReference_DISABLED :: XavcGopBReference
pattern XavcGopBReference_DISABLED = XavcGopBReference' "DISABLED"

pattern XavcGopBReference_ENABLED :: XavcGopBReference
pattern XavcGopBReference_ENABLED = XavcGopBReference' "ENABLED"

{-# COMPLETE
  XavcGopBReference_DISABLED,
  XavcGopBReference_ENABLED,
  XavcGopBReference'
  #-}

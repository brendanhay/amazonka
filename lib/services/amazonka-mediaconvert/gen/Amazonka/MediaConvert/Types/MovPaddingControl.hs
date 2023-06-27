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
-- Module      : Amazonka.MediaConvert.Types.MovPaddingControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MovPaddingControl
  ( MovPaddingControl
      ( ..,
        MovPaddingControl_NONE,
        MovPaddingControl_OMNEON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Unless you need Omneon compatibility: Keep the default value, None. To
-- make this output compatible with Omneon: Choose Omneon. When you do,
-- MediaConvert increases the length of the \'elst\' edit list atom. Note
-- that this might cause file rejections when a recipient of the output
-- file doesn\'t expect this extra padding.
newtype MovPaddingControl = MovPaddingControl'
  { fromMovPaddingControl ::
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

pattern MovPaddingControl_NONE :: MovPaddingControl
pattern MovPaddingControl_NONE = MovPaddingControl' "NONE"

pattern MovPaddingControl_OMNEON :: MovPaddingControl
pattern MovPaddingControl_OMNEON = MovPaddingControl' "OMNEON"

{-# COMPLETE
  MovPaddingControl_NONE,
  MovPaddingControl_OMNEON,
  MovPaddingControl'
  #-}

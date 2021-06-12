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
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Syntax
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2Syntax
  ( Mpeg2Syntax
      ( ..,
        Mpeg2Syntax_DEFAULT,
        Mpeg2Syntax_D_10
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Specify whether this output\'s video uses the D10 syntax. Keep the
-- default value to not use the syntax. Related settings: When you choose
-- D10 (D_10) for your MXF profile (profile), you must also set this value
-- to to D10 (D_10).
newtype Mpeg2Syntax = Mpeg2Syntax'
  { fromMpeg2Syntax ::
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

pattern Mpeg2Syntax_DEFAULT :: Mpeg2Syntax
pattern Mpeg2Syntax_DEFAULT = Mpeg2Syntax' "DEFAULT"

pattern Mpeg2Syntax_D_10 :: Mpeg2Syntax
pattern Mpeg2Syntax_D_10 = Mpeg2Syntax' "D_10"

{-# COMPLETE
  Mpeg2Syntax_DEFAULT,
  Mpeg2Syntax_D_10,
  Mpeg2Syntax'
  #-}

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
-- Module      : Network.AWS.MediaConvert.Types.OutputSdt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputSdt
  ( OutputSdt
      ( ..,
        OutputSdt_SDT_FOLLOW,
        OutputSdt_SDT_FOLLOW_IF_PRESENT,
        OutputSdt_SDT_MANUAL,
        OutputSdt_SDT_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Selects method of inserting SDT information into output stream. \"Follow
-- input SDT\" copies SDT information from input stream to output stream.
-- \"Follow input SDT if present\" copies SDT information from input stream
-- to output stream if SDT information is present in the input, otherwise
-- it will fall back on the user-defined values. Enter \"SDT Manually\"
-- means user will enter the SDT information. \"No SDT\" means output
-- stream will not contain SDT information.
newtype OutputSdt = OutputSdt'
  { fromOutputSdt ::
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

pattern OutputSdt_SDT_FOLLOW :: OutputSdt
pattern OutputSdt_SDT_FOLLOW = OutputSdt' "SDT_FOLLOW"

pattern OutputSdt_SDT_FOLLOW_IF_PRESENT :: OutputSdt
pattern OutputSdt_SDT_FOLLOW_IF_PRESENT = OutputSdt' "SDT_FOLLOW_IF_PRESENT"

pattern OutputSdt_SDT_MANUAL :: OutputSdt
pattern OutputSdt_SDT_MANUAL = OutputSdt' "SDT_MANUAL"

pattern OutputSdt_SDT_NONE :: OutputSdt
pattern OutputSdt_SDT_NONE = OutputSdt' "SDT_NONE"

{-# COMPLETE
  OutputSdt_SDT_FOLLOW,
  OutputSdt_SDT_FOLLOW_IF_PRESENT,
  OutputSdt_SDT_MANUAL,
  OutputSdt_SDT_NONE,
  OutputSdt'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputSdt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputSdt
  ( OutputSdt
      ( OutputSdt',
        OutputSdtSdtFollow,
        OutputSdtSdtFollowIfPresent,
        OutputSdtSdtManual,
        OutputSdtSdtNone,
        fromOutputSdt
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.
newtype OutputSdt = OutputSdt' {fromOutputSdt :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern OutputSdtSdtFollow :: OutputSdt
pattern OutputSdtSdtFollow = OutputSdt' "SDT_FOLLOW"

pattern OutputSdtSdtFollowIfPresent :: OutputSdt
pattern OutputSdtSdtFollowIfPresent = OutputSdt' "SDT_FOLLOW_IF_PRESENT"

pattern OutputSdtSdtManual :: OutputSdt
pattern OutputSdtSdtManual = OutputSdt' "SDT_MANUAL"

pattern OutputSdtSdtNone :: OutputSdt
pattern OutputSdtSdtNone = OutputSdt' "SDT_NONE"

{-# COMPLETE
  OutputSdtSdtFollow,
  OutputSdtSdtFollowIfPresent,
  OutputSdtSdtManual,
  OutputSdtSdtNone,
  OutputSdt'
  #-}

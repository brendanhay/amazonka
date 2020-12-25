{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265GopBReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265GopBReference
  ( H265GopBReference
      ( H265GopBReference',
        H265GopBReferenceDisabled,
        H265GopBReferenceEnabled,
        fromH265GopBReference
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
newtype H265GopBReference = H265GopBReference'
  { fromH265GopBReference ::
      Core.Text
  }
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

pattern H265GopBReferenceDisabled :: H265GopBReference
pattern H265GopBReferenceDisabled = H265GopBReference' "DISABLED"

pattern H265GopBReferenceEnabled :: H265GopBReference
pattern H265GopBReferenceEnabled = H265GopBReference' "ENABLED"

{-# COMPLETE
  H265GopBReferenceDisabled,
  H265GopBReferenceEnabled,
  H265GopBReference'
  #-}

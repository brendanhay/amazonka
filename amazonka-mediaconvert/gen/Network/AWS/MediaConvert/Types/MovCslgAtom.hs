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
-- Module      : Network.AWS.MediaConvert.Types.MovCslgAtom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovCslgAtom
  ( MovCslgAtom
      ( ..,
        MovCslgAtom_EXCLUDE,
        MovCslgAtom_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
newtype MovCslgAtom = MovCslgAtom'
  { fromMovCslgAtom ::
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

pattern MovCslgAtom_EXCLUDE :: MovCslgAtom
pattern MovCslgAtom_EXCLUDE = MovCslgAtom' "EXCLUDE"

pattern MovCslgAtom_INCLUDE :: MovCslgAtom
pattern MovCslgAtom_INCLUDE = MovCslgAtom' "INCLUDE"

{-# COMPLETE
  MovCslgAtom_EXCLUDE,
  MovCslgAtom_INCLUDE,
  MovCslgAtom'
  #-}

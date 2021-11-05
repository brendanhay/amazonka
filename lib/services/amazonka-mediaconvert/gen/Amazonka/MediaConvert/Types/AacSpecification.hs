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
-- Module      : Amazonka.MediaConvert.Types.AacSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AacSpecification
  ( AacSpecification
      ( ..,
        AacSpecification_MPEG2,
        AacSpecification_MPEG4
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport
-- Stream containers.
newtype AacSpecification = AacSpecification'
  { fromAacSpecification ::
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

pattern AacSpecification_MPEG2 :: AacSpecification
pattern AacSpecification_MPEG2 = AacSpecification' "MPEG2"

pattern AacSpecification_MPEG4 :: AacSpecification
pattern AacSpecification_MPEG4 = AacSpecification' "MPEG4"

{-# COMPLETE
  AacSpecification_MPEG2,
  AacSpecification_MPEG4,
  AacSpecification'
  #-}

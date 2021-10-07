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
-- Module      : Network.AWS.Rekognition.Types.BodyPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.BodyPart
  ( BodyPart
      ( ..,
        BodyPart_FACE,
        BodyPart_HEAD,
        BodyPart_LEFT_HAND,
        BodyPart_RIGHT_HAND
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BodyPart = BodyPart'
  { fromBodyPart ::
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

pattern BodyPart_FACE :: BodyPart
pattern BodyPart_FACE = BodyPart' "FACE"

pattern BodyPart_HEAD :: BodyPart
pattern BodyPart_HEAD = BodyPart' "HEAD"

pattern BodyPart_LEFT_HAND :: BodyPart
pattern BodyPart_LEFT_HAND = BodyPart' "LEFT_HAND"

pattern BodyPart_RIGHT_HAND :: BodyPart
pattern BodyPart_RIGHT_HAND = BodyPart' "RIGHT_HAND"

{-# COMPLETE
  BodyPart_FACE,
  BodyPart_HEAD,
  BodyPart_LEFT_HAND,
  BodyPart_RIGHT_HAND,
  BodyPart'
  #-}

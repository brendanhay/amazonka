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
-- Module      : Amazonka.Rekognition.Types.BodyPart
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.BodyPart
  ( BodyPart
      ( ..,
        BodyPart_FACE,
        BodyPart_HEAD,
        BodyPart_LEFT_HAND,
        BodyPart_RIGHT_HAND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BodyPart = BodyPart'
  { fromBodyPart ::
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

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
-- Module      : Network.AWS.MediaLive.Types.M2tsBufferModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsBufferModel
  ( M2tsBufferModel
      ( ..,
        M2tsBufferModel_MULTIPLEX,
        M2tsBufferModel_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | M2ts Buffer Model
newtype M2tsBufferModel = M2tsBufferModel'
  { fromM2tsBufferModel ::
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

pattern M2tsBufferModel_MULTIPLEX :: M2tsBufferModel
pattern M2tsBufferModel_MULTIPLEX = M2tsBufferModel' "MULTIPLEX"

pattern M2tsBufferModel_NONE :: M2tsBufferModel
pattern M2tsBufferModel_NONE = M2tsBufferModel' "NONE"

{-# COMPLETE
  M2tsBufferModel_MULTIPLEX,
  M2tsBufferModel_NONE,
  M2tsBufferModel'
  #-}

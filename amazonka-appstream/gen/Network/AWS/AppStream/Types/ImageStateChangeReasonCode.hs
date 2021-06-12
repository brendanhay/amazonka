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
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageStateChangeReasonCode
  ( ImageStateChangeReasonCode
      ( ..,
        ImageStateChangeReasonCode_IMAGE_BUILDER_NOT_AVAILABLE,
        ImageStateChangeReasonCode_IMAGE_COPY_FAILURE,
        ImageStateChangeReasonCode_INTERNAL_ERROR
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ImageStateChangeReasonCode = ImageStateChangeReasonCode'
  { fromImageStateChangeReasonCode ::
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

pattern ImageStateChangeReasonCode_IMAGE_BUILDER_NOT_AVAILABLE :: ImageStateChangeReasonCode
pattern ImageStateChangeReasonCode_IMAGE_BUILDER_NOT_AVAILABLE = ImageStateChangeReasonCode' "IMAGE_BUILDER_NOT_AVAILABLE"

pattern ImageStateChangeReasonCode_IMAGE_COPY_FAILURE :: ImageStateChangeReasonCode
pattern ImageStateChangeReasonCode_IMAGE_COPY_FAILURE = ImageStateChangeReasonCode' "IMAGE_COPY_FAILURE"

pattern ImageStateChangeReasonCode_INTERNAL_ERROR :: ImageStateChangeReasonCode
pattern ImageStateChangeReasonCode_INTERNAL_ERROR = ImageStateChangeReasonCode' "INTERNAL_ERROR"

{-# COMPLETE
  ImageStateChangeReasonCode_IMAGE_BUILDER_NOT_AVAILABLE,
  ImageStateChangeReasonCode_IMAGE_COPY_FAILURE,
  ImageStateChangeReasonCode_INTERNAL_ERROR,
  ImageStateChangeReasonCode'
  #-}

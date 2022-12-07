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
-- Module      : Amazonka.AppStream.Types.ImageStateChangeReasonCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ImageStateChangeReasonCode
  ( ImageStateChangeReasonCode
      ( ..,
        ImageStateChangeReasonCode_IMAGE_BUILDER_NOT_AVAILABLE,
        ImageStateChangeReasonCode_IMAGE_COPY_FAILURE,
        ImageStateChangeReasonCode_INTERNAL_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageStateChangeReasonCode = ImageStateChangeReasonCode'
  { fromImageStateChangeReasonCode ::
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

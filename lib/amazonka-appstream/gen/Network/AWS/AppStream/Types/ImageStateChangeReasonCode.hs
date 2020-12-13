{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageStateChangeReasonCode
  ( ImageStateChangeReasonCode
      ( ImageStateChangeReasonCode',
        InternalError,
        ImageBuilderNotAvailable,
        ImageCopyFailure
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ImageStateChangeReasonCode = ImageStateChangeReasonCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern InternalError :: ImageStateChangeReasonCode
pattern InternalError = ImageStateChangeReasonCode' "INTERNAL_ERROR"

pattern ImageBuilderNotAvailable :: ImageStateChangeReasonCode
pattern ImageBuilderNotAvailable = ImageStateChangeReasonCode' "IMAGE_BUILDER_NOT_AVAILABLE"

pattern ImageCopyFailure :: ImageStateChangeReasonCode
pattern ImageCopyFailure = ImageStateChangeReasonCode' "IMAGE_COPY_FAILURE"

{-# COMPLETE
  InternalError,
  ImageBuilderNotAvailable,
  ImageCopyFailure,
  ImageStateChangeReasonCode'
  #-}

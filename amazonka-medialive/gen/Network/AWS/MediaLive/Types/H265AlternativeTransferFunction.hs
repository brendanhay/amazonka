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
-- Module      : Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
  ( H265AlternativeTransferFunction
      ( ..,
        H265AlternativeTransferFunction_INSERT,
        H265AlternativeTransferFunction_OMIT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H265 Alternative Transfer Function
newtype H265AlternativeTransferFunction = H265AlternativeTransferFunction'
  { fromH265AlternativeTransferFunction ::
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

pattern H265AlternativeTransferFunction_INSERT :: H265AlternativeTransferFunction
pattern H265AlternativeTransferFunction_INSERT = H265AlternativeTransferFunction' "INSERT"

pattern H265AlternativeTransferFunction_OMIT :: H265AlternativeTransferFunction
pattern H265AlternativeTransferFunction_OMIT = H265AlternativeTransferFunction' "OMIT"

{-# COMPLETE
  H265AlternativeTransferFunction_INSERT,
  H265AlternativeTransferFunction_OMIT,
  H265AlternativeTransferFunction'
  #-}

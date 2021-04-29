{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | H265 Alternative Transfer Function
newtype H265AlternativeTransferFunction = H265AlternativeTransferFunction'
  { fromH265AlternativeTransferFunction ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

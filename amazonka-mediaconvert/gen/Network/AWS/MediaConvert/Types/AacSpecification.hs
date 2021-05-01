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
-- Module      : Network.AWS.MediaConvert.Types.AacSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacSpecification
  ( AacSpecification
      ( ..,
        AacSpecification_MPEG2,
        AacSpecification_MPEG4
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport
-- Stream containers.
newtype AacSpecification = AacSpecification'
  { fromAacSpecification ::
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

pattern AacSpecification_MPEG2 :: AacSpecification
pattern AacSpecification_MPEG2 = AacSpecification' "MPEG2"

pattern AacSpecification_MPEG4 :: AacSpecification
pattern AacSpecification_MPEG4 = AacSpecification' "MPEG4"

{-# COMPLETE
  AacSpecification_MPEG2,
  AacSpecification_MPEG4,
  AacSpecification'
  #-}

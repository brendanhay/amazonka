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
-- Module      : Network.AWS.MediaConvert.Types.Mp3RateControlMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp3RateControlMode
  ( Mp3RateControlMode
      ( ..,
        Mp3RateControlMode_CBR,
        Mp3RateControlMode_VBR
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specify whether the service encodes this MP3 audio output with a
-- constant bitrate (CBR) or a variable bitrate (VBR).
newtype Mp3RateControlMode = Mp3RateControlMode'
  { fromMp3RateControlMode ::
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

pattern Mp3RateControlMode_CBR :: Mp3RateControlMode
pattern Mp3RateControlMode_CBR = Mp3RateControlMode' "CBR"

pattern Mp3RateControlMode_VBR :: Mp3RateControlMode
pattern Mp3RateControlMode_VBR = Mp3RateControlMode' "VBR"

{-# COMPLETE
  Mp3RateControlMode_CBR,
  Mp3RateControlMode_VBR,
  Mp3RateControlMode'
  #-}

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
-- Module      : Network.AWS.MediaConvert.Types.H265RateControlMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265RateControlMode
  ( H265RateControlMode
      ( ..,
        H265RateControlMode_CBR,
        H265RateControlMode_QVBR,
        H265RateControlMode_VBR
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Use this setting to specify whether this output has a variable bitrate
-- (VBR), constant bitrate (CBR) or quality-defined variable bitrate
-- (QVBR).
newtype H265RateControlMode = H265RateControlMode'
  { fromH265RateControlMode ::
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

pattern H265RateControlMode_CBR :: H265RateControlMode
pattern H265RateControlMode_CBR = H265RateControlMode' "CBR"

pattern H265RateControlMode_QVBR :: H265RateControlMode
pattern H265RateControlMode_QVBR = H265RateControlMode' "QVBR"

pattern H265RateControlMode_VBR :: H265RateControlMode
pattern H265RateControlMode_VBR = H265RateControlMode' "VBR"

{-# COMPLETE
  H265RateControlMode_CBR,
  H265RateControlMode_QVBR,
  H265RateControlMode_VBR,
  H265RateControlMode'
  #-}

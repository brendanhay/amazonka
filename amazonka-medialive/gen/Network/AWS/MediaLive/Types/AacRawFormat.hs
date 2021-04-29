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
-- Module      : Network.AWS.MediaLive.Types.AacRawFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacRawFormat
  ( AacRawFormat
      ( ..,
        AacRawFormat_LATM_LOAS,
        AacRawFormat_NONE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Aac Raw Format
newtype AacRawFormat = AacRawFormat'
  { fromAacRawFormat ::
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

pattern AacRawFormat_LATM_LOAS :: AacRawFormat
pattern AacRawFormat_LATM_LOAS = AacRawFormat' "LATM_LOAS"

pattern AacRawFormat_NONE :: AacRawFormat
pattern AacRawFormat_NONE = AacRawFormat' "NONE"

{-# COMPLETE
  AacRawFormat_LATM_LOAS,
  AacRawFormat_NONE,
  AacRawFormat'
  #-}

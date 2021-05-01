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
-- Module      : Network.AWS.MediaConvert.Types.CmafStreamInfResolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafStreamInfResolution
  ( CmafStreamInfResolution
      ( ..,
        CmafStreamInfResolution_EXCLUDE,
        CmafStreamInfResolution_INCLUDE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
newtype CmafStreamInfResolution = CmafStreamInfResolution'
  { fromCmafStreamInfResolution ::
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

pattern CmafStreamInfResolution_EXCLUDE :: CmafStreamInfResolution
pattern CmafStreamInfResolution_EXCLUDE = CmafStreamInfResolution' "EXCLUDE"

pattern CmafStreamInfResolution_INCLUDE :: CmafStreamInfResolution
pattern CmafStreamInfResolution_INCLUDE = CmafStreamInfResolution' "INCLUDE"

{-# COMPLETE
  CmafStreamInfResolution_EXCLUDE,
  CmafStreamInfResolution_INCLUDE,
  CmafStreamInfResolution'
  #-}

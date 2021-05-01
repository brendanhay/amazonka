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
-- Module      : Network.AWS.MediaConvert.Types.Ac3LfeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3LfeFilter
  ( Ac3LfeFilter
      ( ..,
        Ac3LfeFilter_DISABLED,
        Ac3LfeFilter_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
newtype Ac3LfeFilter = Ac3LfeFilter'
  { fromAc3LfeFilter ::
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

pattern Ac3LfeFilter_DISABLED :: Ac3LfeFilter
pattern Ac3LfeFilter_DISABLED = Ac3LfeFilter' "DISABLED"

pattern Ac3LfeFilter_ENABLED :: Ac3LfeFilter
pattern Ac3LfeFilter_ENABLED = Ac3LfeFilter' "ENABLED"

{-# COMPLETE
  Ac3LfeFilter_DISABLED,
  Ac3LfeFilter_ENABLED,
  Ac3LfeFilter'
  #-}

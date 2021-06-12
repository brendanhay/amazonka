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
-- Module      : Network.AWS.MediaLive.Types.Eac3DcFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3DcFilter
  ( Eac3DcFilter
      ( ..,
        Eac3DcFilter_DISABLED,
        Eac3DcFilter_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Eac3 Dc Filter
newtype Eac3DcFilter = Eac3DcFilter'
  { fromEac3DcFilter ::
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

pattern Eac3DcFilter_DISABLED :: Eac3DcFilter
pattern Eac3DcFilter_DISABLED = Eac3DcFilter' "DISABLED"

pattern Eac3DcFilter_ENABLED :: Eac3DcFilter
pattern Eac3DcFilter_ENABLED = Eac3DcFilter' "ENABLED"

{-# COMPLETE
  Eac3DcFilter_DISABLED,
  Eac3DcFilter_ENABLED,
  Eac3DcFilter'
  #-}

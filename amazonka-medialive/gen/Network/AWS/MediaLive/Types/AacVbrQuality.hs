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
-- Module      : Network.AWS.MediaLive.Types.AacVbrQuality
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacVbrQuality
  ( AacVbrQuality
      ( ..,
        AacVbrQuality_HIGH,
        AacVbrQuality_LOW,
        AacVbrQuality_MEDIUM_HIGH,
        AacVbrQuality_MEDIUM_LOW
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Aac Vbr Quality
newtype AacVbrQuality = AacVbrQuality'
  { fromAacVbrQuality ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern AacVbrQuality_HIGH :: AacVbrQuality
pattern AacVbrQuality_HIGH = AacVbrQuality' "HIGH"

pattern AacVbrQuality_LOW :: AacVbrQuality
pattern AacVbrQuality_LOW = AacVbrQuality' "LOW"

pattern AacVbrQuality_MEDIUM_HIGH :: AacVbrQuality
pattern AacVbrQuality_MEDIUM_HIGH = AacVbrQuality' "MEDIUM_HIGH"

pattern AacVbrQuality_MEDIUM_LOW :: AacVbrQuality
pattern AacVbrQuality_MEDIUM_LOW = AacVbrQuality' "MEDIUM_LOW"

{-# COMPLETE
  AacVbrQuality_HIGH,
  AacVbrQuality_LOW,
  AacVbrQuality_MEDIUM_HIGH,
  AacVbrQuality_MEDIUM_LOW,
  AacVbrQuality'
  #-}

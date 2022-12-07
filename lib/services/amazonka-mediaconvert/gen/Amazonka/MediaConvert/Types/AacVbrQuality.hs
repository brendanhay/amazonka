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
-- Module      : Amazonka.MediaConvert.Types.AacVbrQuality
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AacVbrQuality
  ( AacVbrQuality
      ( ..,
        AacVbrQuality_HIGH,
        AacVbrQuality_LOW,
        AacVbrQuality_MEDIUM_HIGH,
        AacVbrQuality_MEDIUM_LOW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
newtype AacVbrQuality = AacVbrQuality'
  { fromAacVbrQuality ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

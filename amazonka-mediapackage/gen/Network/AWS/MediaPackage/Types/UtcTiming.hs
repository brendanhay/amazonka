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
-- Module      : Network.AWS.MediaPackage.Types.UtcTiming
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.UtcTiming
  ( UtcTiming
      ( ..,
        UtcTiming_HTTP_HEAD,
        UtcTiming_HTTP_ISO,
        UtcTiming_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype UtcTiming = UtcTiming'
  { fromUtcTiming ::
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

pattern UtcTiming_HTTP_HEAD :: UtcTiming
pattern UtcTiming_HTTP_HEAD = UtcTiming' "HTTP-HEAD"

pattern UtcTiming_HTTP_ISO :: UtcTiming
pattern UtcTiming_HTTP_ISO = UtcTiming' "HTTP-ISO"

pattern UtcTiming_NONE :: UtcTiming
pattern UtcTiming_NONE = UtcTiming' "NONE"

{-# COMPLETE
  UtcTiming_HTTP_HEAD,
  UtcTiming_HTTP_ISO,
  UtcTiming_NONE,
  UtcTiming'
  #-}

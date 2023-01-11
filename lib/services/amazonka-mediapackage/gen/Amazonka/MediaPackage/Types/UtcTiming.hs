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
-- Module      : Amazonka.MediaPackage.Types.UtcTiming
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.UtcTiming
  ( UtcTiming
      ( ..,
        UtcTiming_HTTP_HEAD,
        UtcTiming_HTTP_ISO,
        UtcTiming_HTTP_XSDATE,
        UtcTiming_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UtcTiming = UtcTiming'
  { fromUtcTiming ::
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

pattern UtcTiming_HTTP_HEAD :: UtcTiming
pattern UtcTiming_HTTP_HEAD = UtcTiming' "HTTP-HEAD"

pattern UtcTiming_HTTP_ISO :: UtcTiming
pattern UtcTiming_HTTP_ISO = UtcTiming' "HTTP-ISO"

pattern UtcTiming_HTTP_XSDATE :: UtcTiming
pattern UtcTiming_HTTP_XSDATE = UtcTiming' "HTTP-XSDATE"

pattern UtcTiming_NONE :: UtcTiming
pattern UtcTiming_NONE = UtcTiming' "NONE"

{-# COMPLETE
  UtcTiming_HTTP_HEAD,
  UtcTiming_HTTP_ISO,
  UtcTiming_HTTP_XSDATE,
  UtcTiming_NONE,
  UtcTiming'
  #-}

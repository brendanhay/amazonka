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

import qualified Network.AWS.Prelude as Prelude

newtype UtcTiming = UtcTiming'
  { fromUtcTiming ::
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

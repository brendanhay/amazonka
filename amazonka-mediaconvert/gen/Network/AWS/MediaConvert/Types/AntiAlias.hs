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
-- Module      : Network.AWS.MediaConvert.Types.AntiAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AntiAlias
  ( AntiAlias
      ( ..,
        AntiAlias_DISABLED,
        AntiAlias_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The anti-alias filter is automatically applied to all outputs. The
-- service no longer accepts the value DISABLED for AntiAlias. If you
-- specify that in your job, the service will ignore the setting.
newtype AntiAlias = AntiAlias'
  { fromAntiAlias ::
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

pattern AntiAlias_DISABLED :: AntiAlias
pattern AntiAlias_DISABLED = AntiAlias' "DISABLED"

pattern AntiAlias_ENABLED :: AntiAlias
pattern AntiAlias_ENABLED = AntiAlias' "ENABLED"

{-# COMPLETE
  AntiAlias_DISABLED,
  AntiAlias_ENABLED,
  AntiAlias'
  #-}

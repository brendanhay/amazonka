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
-- Module      : Amazonka.MediaLive.Types.M2tsCcDescriptor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M2tsCcDescriptor
  ( M2tsCcDescriptor
      ( ..,
        M2tsCcDescriptor_DISABLED,
        M2tsCcDescriptor_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | M2ts Cc Descriptor
newtype M2tsCcDescriptor = M2tsCcDescriptor'
  { fromM2tsCcDescriptor ::
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

pattern M2tsCcDescriptor_DISABLED :: M2tsCcDescriptor
pattern M2tsCcDescriptor_DISABLED = M2tsCcDescriptor' "DISABLED"

pattern M2tsCcDescriptor_ENABLED :: M2tsCcDescriptor
pattern M2tsCcDescriptor_ENABLED = M2tsCcDescriptor' "ENABLED"

{-# COMPLETE
  M2tsCcDescriptor_DISABLED,
  M2tsCcDescriptor_ENABLED,
  M2tsCcDescriptor'
  #-}

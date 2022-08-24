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
-- Module      : Amazonka.Nimble.Types.StudioComponentSubtype
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentSubtype
  ( StudioComponentSubtype
      ( ..,
        StudioComponentSubtype_AMAZON_FSX_FOR_LUSTRE,
        StudioComponentSubtype_AMAZON_FSX_FOR_WINDOWS,
        StudioComponentSubtype_AWS_MANAGED_MICROSOFT_AD,
        StudioComponentSubtype_CUSTOM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StudioComponentSubtype = StudioComponentSubtype'
  { fromStudioComponentSubtype ::
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

pattern StudioComponentSubtype_AMAZON_FSX_FOR_LUSTRE :: StudioComponentSubtype
pattern StudioComponentSubtype_AMAZON_FSX_FOR_LUSTRE = StudioComponentSubtype' "AMAZON_FSX_FOR_LUSTRE"

pattern StudioComponentSubtype_AMAZON_FSX_FOR_WINDOWS :: StudioComponentSubtype
pattern StudioComponentSubtype_AMAZON_FSX_FOR_WINDOWS = StudioComponentSubtype' "AMAZON_FSX_FOR_WINDOWS"

pattern StudioComponentSubtype_AWS_MANAGED_MICROSOFT_AD :: StudioComponentSubtype
pattern StudioComponentSubtype_AWS_MANAGED_MICROSOFT_AD = StudioComponentSubtype' "AWS_MANAGED_MICROSOFT_AD"

pattern StudioComponentSubtype_CUSTOM :: StudioComponentSubtype
pattern StudioComponentSubtype_CUSTOM = StudioComponentSubtype' "CUSTOM"

{-# COMPLETE
  StudioComponentSubtype_AMAZON_FSX_FOR_LUSTRE,
  StudioComponentSubtype_AMAZON_FSX_FOR_WINDOWS,
  StudioComponentSubtype_AWS_MANAGED_MICROSOFT_AD,
  StudioComponentSubtype_CUSTOM,
  StudioComponentSubtype'
  #-}

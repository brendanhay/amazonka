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
-- Module      : Amazonka.MediaPackageV2.Types.ScteFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.ScteFilter
  ( ScteFilter
      ( ..,
        ScteFilter_BREAK,
        ScteFilter_DISTRIBUTOR_ADVERTISEMENT,
        ScteFilter_DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY,
        ScteFilter_DISTRIBUTOR_PLACEMENT_OPPORTUNITY,
        ScteFilter_PROGRAM,
        ScteFilter_PROVIDER_ADVERTISEMENT,
        ScteFilter_PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY,
        ScteFilter_PROVIDER_PLACEMENT_OPPORTUNITY,
        ScteFilter_SPLICE_INSERT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScteFilter = ScteFilter'
  { fromScteFilter ::
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

pattern ScteFilter_BREAK :: ScteFilter
pattern ScteFilter_BREAK = ScteFilter' "BREAK"

pattern ScteFilter_DISTRIBUTOR_ADVERTISEMENT :: ScteFilter
pattern ScteFilter_DISTRIBUTOR_ADVERTISEMENT = ScteFilter' "DISTRIBUTOR_ADVERTISEMENT"

pattern ScteFilter_DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY :: ScteFilter
pattern ScteFilter_DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY = ScteFilter' "DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern ScteFilter_DISTRIBUTOR_PLACEMENT_OPPORTUNITY :: ScteFilter
pattern ScteFilter_DISTRIBUTOR_PLACEMENT_OPPORTUNITY = ScteFilter' "DISTRIBUTOR_PLACEMENT_OPPORTUNITY"

pattern ScteFilter_PROGRAM :: ScteFilter
pattern ScteFilter_PROGRAM = ScteFilter' "PROGRAM"

pattern ScteFilter_PROVIDER_ADVERTISEMENT :: ScteFilter
pattern ScteFilter_PROVIDER_ADVERTISEMENT = ScteFilter' "PROVIDER_ADVERTISEMENT"

pattern ScteFilter_PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY :: ScteFilter
pattern ScteFilter_PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY = ScteFilter' "PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern ScteFilter_PROVIDER_PLACEMENT_OPPORTUNITY :: ScteFilter
pattern ScteFilter_PROVIDER_PLACEMENT_OPPORTUNITY = ScteFilter' "PROVIDER_PLACEMENT_OPPORTUNITY"

pattern ScteFilter_SPLICE_INSERT :: ScteFilter
pattern ScteFilter_SPLICE_INSERT = ScteFilter' "SPLICE_INSERT"

{-# COMPLETE
  ScteFilter_BREAK,
  ScteFilter_DISTRIBUTOR_ADVERTISEMENT,
  ScteFilter_DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY,
  ScteFilter_DISTRIBUTOR_PLACEMENT_OPPORTUNITY,
  ScteFilter_PROGRAM,
  ScteFilter_PROVIDER_ADVERTISEMENT,
  ScteFilter_PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY,
  ScteFilter_PROVIDER_PLACEMENT_OPPORTUNITY,
  ScteFilter_SPLICE_INSERT,
  ScteFilter'
  #-}

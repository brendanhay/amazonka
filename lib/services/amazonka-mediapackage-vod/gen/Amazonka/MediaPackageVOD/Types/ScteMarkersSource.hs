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
-- Module      : Amazonka.MediaPackageVOD.Types.ScteMarkersSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.ScteMarkersSource
  ( ScteMarkersSource
      ( ..,
        ScteMarkersSource_MANIFEST,
        ScteMarkersSource_SEGMENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScteMarkersSource = ScteMarkersSource'
  { fromScteMarkersSource ::
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

pattern ScteMarkersSource_MANIFEST :: ScteMarkersSource
pattern ScteMarkersSource_MANIFEST = ScteMarkersSource' "MANIFEST"

pattern ScteMarkersSource_SEGMENTS :: ScteMarkersSource
pattern ScteMarkersSource_SEGMENTS = ScteMarkersSource' "SEGMENTS"

{-# COMPLETE
  ScteMarkersSource_MANIFEST,
  ScteMarkersSource_SEGMENTS,
  ScteMarkersSource'
  #-}

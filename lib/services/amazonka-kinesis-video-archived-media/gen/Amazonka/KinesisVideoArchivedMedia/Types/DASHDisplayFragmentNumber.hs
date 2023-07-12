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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
  ( DASHDisplayFragmentNumber
      ( ..,
        DASHDisplayFragmentNumber_ALWAYS,
        DASHDisplayFragmentNumber_NEVER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DASHDisplayFragmentNumber = DASHDisplayFragmentNumber'
  { fromDASHDisplayFragmentNumber ::
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

pattern DASHDisplayFragmentNumber_ALWAYS :: DASHDisplayFragmentNumber
pattern DASHDisplayFragmentNumber_ALWAYS = DASHDisplayFragmentNumber' "ALWAYS"

pattern DASHDisplayFragmentNumber_NEVER :: DASHDisplayFragmentNumber
pattern DASHDisplayFragmentNumber_NEVER = DASHDisplayFragmentNumber' "NEVER"

{-# COMPLETE
  DASHDisplayFragmentNumber_ALWAYS,
  DASHDisplayFragmentNumber_NEVER,
  DASHDisplayFragmentNumber'
  #-}

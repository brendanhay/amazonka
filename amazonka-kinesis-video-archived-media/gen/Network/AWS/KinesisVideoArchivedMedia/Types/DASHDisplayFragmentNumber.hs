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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
  ( DASHDisplayFragmentNumber
      ( ..,
        DASHDisplayFragmentNumber_ALWAYS,
        DASHDisplayFragmentNumber_NEVER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DASHDisplayFragmentNumber = DASHDisplayFragmentNumber'
  { fromDASHDisplayFragmentNumber ::
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

pattern DASHDisplayFragmentNumber_ALWAYS :: DASHDisplayFragmentNumber
pattern DASHDisplayFragmentNumber_ALWAYS = DASHDisplayFragmentNumber' "ALWAYS"

pattern DASHDisplayFragmentNumber_NEVER :: DASHDisplayFragmentNumber
pattern DASHDisplayFragmentNumber_NEVER = DASHDisplayFragmentNumber' "NEVER"

{-# COMPLETE
  DASHDisplayFragmentNumber_ALWAYS,
  DASHDisplayFragmentNumber_NEVER,
  DASHDisplayFragmentNumber'
  #-}

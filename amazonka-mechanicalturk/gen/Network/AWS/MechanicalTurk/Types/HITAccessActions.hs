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
-- Module      : Network.AWS.MechanicalTurk.Types.HITAccessActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITAccessActions
  ( HITAccessActions
      ( ..,
        HITAccessActions_Accept,
        HITAccessActions_DiscoverPreviewAndAccept,
        HITAccessActions_PreviewAndAccept
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype HITAccessActions = HITAccessActions'
  { fromHITAccessActions ::
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

pattern HITAccessActions_Accept :: HITAccessActions
pattern HITAccessActions_Accept = HITAccessActions' "Accept"

pattern HITAccessActions_DiscoverPreviewAndAccept :: HITAccessActions
pattern HITAccessActions_DiscoverPreviewAndAccept = HITAccessActions' "DiscoverPreviewAndAccept"

pattern HITAccessActions_PreviewAndAccept :: HITAccessActions
pattern HITAccessActions_PreviewAndAccept = HITAccessActions' "PreviewAndAccept"

{-# COMPLETE
  HITAccessActions_Accept,
  HITAccessActions_DiscoverPreviewAndAccept,
  HITAccessActions_PreviewAndAccept,
  HITAccessActions'
  #-}

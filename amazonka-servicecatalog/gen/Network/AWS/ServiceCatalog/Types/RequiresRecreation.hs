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
-- Module      : Network.AWS.ServiceCatalog.Types.RequiresRecreation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RequiresRecreation
  ( RequiresRecreation
      ( ..,
        RequiresRecreation_ALWAYS,
        RequiresRecreation_CONDITIONALLY,
        RequiresRecreation_NEVER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RequiresRecreation = RequiresRecreation'
  { fromRequiresRecreation ::
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

pattern RequiresRecreation_ALWAYS :: RequiresRecreation
pattern RequiresRecreation_ALWAYS = RequiresRecreation' "ALWAYS"

pattern RequiresRecreation_CONDITIONALLY :: RequiresRecreation
pattern RequiresRecreation_CONDITIONALLY = RequiresRecreation' "CONDITIONALLY"

pattern RequiresRecreation_NEVER :: RequiresRecreation
pattern RequiresRecreation_NEVER = RequiresRecreation' "NEVER"

{-# COMPLETE
  RequiresRecreation_ALWAYS,
  RequiresRecreation_CONDITIONALLY,
  RequiresRecreation_NEVER,
  RequiresRecreation'
  #-}

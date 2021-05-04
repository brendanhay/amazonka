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
-- Module      : Network.AWS.WorkSpaces.Types.Compute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Compute
  ( Compute
      ( ..,
        Compute_GRAPHICS,
        Compute_GRAPHICSPRO,
        Compute_PERFORMANCE,
        Compute_POWER,
        Compute_POWERPRO,
        Compute_STANDARD,
        Compute_VALUE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype Compute = Compute'
  { fromCompute ::
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

pattern Compute_GRAPHICS :: Compute
pattern Compute_GRAPHICS = Compute' "GRAPHICS"

pattern Compute_GRAPHICSPRO :: Compute
pattern Compute_GRAPHICSPRO = Compute' "GRAPHICSPRO"

pattern Compute_PERFORMANCE :: Compute
pattern Compute_PERFORMANCE = Compute' "PERFORMANCE"

pattern Compute_POWER :: Compute
pattern Compute_POWER = Compute' "POWER"

pattern Compute_POWERPRO :: Compute
pattern Compute_POWERPRO = Compute' "POWERPRO"

pattern Compute_STANDARD :: Compute
pattern Compute_STANDARD = Compute' "STANDARD"

pattern Compute_VALUE :: Compute
pattern Compute_VALUE = Compute' "VALUE"

{-# COMPLETE
  Compute_GRAPHICS,
  Compute_GRAPHICSPRO,
  Compute_PERFORMANCE,
  Compute_POWER,
  Compute_POWERPRO,
  Compute_STANDARD,
  Compute_VALUE,
  Compute'
  #-}

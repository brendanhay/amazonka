-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Compute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Compute
  ( Compute
      ( Compute',
        Graphics,
        Graphicspro,
        Performance,
        Power,
        Powerpro,
        Standard,
        Value
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Compute = Compute' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Graphics :: Compute
pattern Graphics = Compute' "GRAPHICS"

pattern Graphicspro :: Compute
pattern Graphicspro = Compute' "GRAPHICSPRO"

pattern Performance :: Compute
pattern Performance = Compute' "PERFORMANCE"

pattern Power :: Compute
pattern Power = Compute' "POWER"

pattern Powerpro :: Compute
pattern Powerpro = Compute' "POWERPRO"

pattern Standard :: Compute
pattern Standard = Compute' "STANDARD"

pattern Value :: Compute
pattern Value = Compute' "VALUE"

{-# COMPLETE
  Graphics,
  Graphicspro,
  Performance,
  Power,
  Powerpro,
  Standard,
  Value,
  Compute'
  #-}

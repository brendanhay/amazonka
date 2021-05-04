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
-- Module      : Network.AWS.Route53Domains.Types.Transferable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.Transferable
  ( Transferable
      ( ..,
        Transferable_DONT_KNOW,
        Transferable_TRANSFERABLE,
        Transferable_UNTRANSFERABLE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Whether the domain name can be transferred to Route 53.
--
-- You can transfer only domains that have a value of @TRANSFERABLE@ for
-- @Transferable@.
--
-- Valid values:
--
-- [TRANSFERABLE]
--     The domain name can be transferred to Route 53.
--
-- [UNTRANSFERRABLE]
--     The domain name can\'t be transferred to Route 53.
--
-- [DONT_KNOW]
--     Reserved for future use.
newtype Transferable = Transferable'
  { fromTransferable ::
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

pattern Transferable_DONT_KNOW :: Transferable
pattern Transferable_DONT_KNOW = Transferable' "DONT_KNOW"

pattern Transferable_TRANSFERABLE :: Transferable
pattern Transferable_TRANSFERABLE = Transferable' "TRANSFERABLE"

pattern Transferable_UNTRANSFERABLE :: Transferable
pattern Transferable_UNTRANSFERABLE = Transferable' "UNTRANSFERABLE"

{-# COMPLETE
  Transferable_DONT_KNOW,
  Transferable_TRANSFERABLE,
  Transferable_UNTRANSFERABLE,
  Transferable'
  #-}

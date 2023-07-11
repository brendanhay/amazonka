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
-- Module      : Amazonka.Route53Domains.Types.Transferable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.Transferable
  ( Transferable
      ( ..,
        Transferable_DOMAIN_IN_ANOTHER_ACCOUNT,
        Transferable_DOMAIN_IN_OWN_ACCOUNT,
        Transferable_DONT_KNOW,
        Transferable_PREMIUM_DOMAIN,
        Transferable_TRANSFERABLE,
        Transferable_UNTRANSFERABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Whether the domain name can be transferred to Route 53.
--
-- You can transfer only domains that have a value of @TRANSFERABLE@ or
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
--
-- [DOMAIN_IN_OWN_ACCOUNT]
--     The domain already exists in the current Amazon Web Services
--     account.
--
-- [DOMAIN_IN_ANOTHER_ACCOUNT]
--     the domain exists in another Amazon Web Services account.
--
-- [PREMIUM_DOMAIN]
--     Premium domain transfer is not supported.
newtype Transferable = Transferable'
  { fromTransferable ::
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

pattern Transferable_DOMAIN_IN_ANOTHER_ACCOUNT :: Transferable
pattern Transferable_DOMAIN_IN_ANOTHER_ACCOUNT = Transferable' "DOMAIN_IN_ANOTHER_ACCOUNT"

pattern Transferable_DOMAIN_IN_OWN_ACCOUNT :: Transferable
pattern Transferable_DOMAIN_IN_OWN_ACCOUNT = Transferable' "DOMAIN_IN_OWN_ACCOUNT"

pattern Transferable_DONT_KNOW :: Transferable
pattern Transferable_DONT_KNOW = Transferable' "DONT_KNOW"

pattern Transferable_PREMIUM_DOMAIN :: Transferable
pattern Transferable_PREMIUM_DOMAIN = Transferable' "PREMIUM_DOMAIN"

pattern Transferable_TRANSFERABLE :: Transferable
pattern Transferable_TRANSFERABLE = Transferable' "TRANSFERABLE"

pattern Transferable_UNTRANSFERABLE :: Transferable
pattern Transferable_UNTRANSFERABLE = Transferable' "UNTRANSFERABLE"

{-# COMPLETE
  Transferable_DOMAIN_IN_ANOTHER_ACCOUNT,
  Transferable_DOMAIN_IN_OWN_ACCOUNT,
  Transferable_DONT_KNOW,
  Transferable_PREMIUM_DOMAIN,
  Transferable_TRANSFERABLE,
  Transferable_UNTRANSFERABLE,
  Transferable'
  #-}

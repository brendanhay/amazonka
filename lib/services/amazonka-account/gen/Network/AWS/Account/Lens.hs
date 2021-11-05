{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Account.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Lens
  ( -- * Operations

    -- ** DeleteAlternateContact
    deleteAlternateContact_accountId,
    deleteAlternateContact_alternateContactType,

    -- ** PutAlternateContact
    putAlternateContact_accountId,
    putAlternateContact_alternateContactType,
    putAlternateContact_emailAddress,
    putAlternateContact_name,
    putAlternateContact_phoneNumber,
    putAlternateContact_title,

    -- ** GetAlternateContact
    getAlternateContact_accountId,
    getAlternateContact_alternateContactType,
    getAlternateContactResponse_alternateContact,
    getAlternateContactResponse_httpStatus,

    -- * Types

    -- ** AlternateContact
    alternateContact_alternateContactType,
    alternateContact_phoneNumber,
    alternateContact_name,
    alternateContact_emailAddress,
    alternateContact_title,
  )
where

import Amazonka.Account.DeleteAlternateContact
import Amazonka.Account.GetAlternateContact
import Amazonka.Account.PutAlternateContact
import Amazonka.Account.Types.AlternateContact

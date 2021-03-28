{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types.SessionToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.STS.Types.SessionToken
  ( SessionToken
    ( SessionToken'
    , fromSessionToken
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SessionToken = SessionToken'{fromSessionToken :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Generic)
                         deriving newtype (Core.Hashable, Core.NFData)

{-# COMPLETE   SessionToken'
  #-}

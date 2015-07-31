{-# LANGUAGE OverloadedStrings #-}

-- Module      : Test.AWS.Data.Base64
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
-- Module      : Test.AWS.Sign.V$
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Sign.V4 (tests) where

import           Data.Monoid
import           Data.String
import           Network.AWS.Prelude
import           Network.HTTP.Types
import           Test.AWS.Util
import           Test.Tasty

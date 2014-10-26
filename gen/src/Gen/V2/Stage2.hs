{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.V2.Stage2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Stage2 where

import Control.Error
import Data.HashMap.Strict (HashMap)
import Data.Jason
import Data.Monoid
import Data.Text           (Text)
import Gen.V2.TH
import Gen.V2.Types

data Stage2 = Stage2

record stage2 ''Stage2

decodeS2 :: Model S2 -> Script Stage2
decodeS2 = const (return Stage2)

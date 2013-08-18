-- |
-- Module      : Network.AWS.Internal
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal
    (
    -- * Re-exported
      module Generics
    , module Internal
    ) where

import GHC.Generics                    as Generics
import Network.HTTP.QueryString.Pickle as Generics
import Text.XML.Expat.Pickle.Generic   as Generics

import Network.AWS.Internal.IO         as Internal
import Network.AWS.Internal.Instances  as Internal
import Network.AWS.Internal.Signing    as Internal
import Network.AWS.Internal.String     as Internal
import Network.AWS.Internal.TH         as Internal
import Network.AWS.Internal.Types      as Internal

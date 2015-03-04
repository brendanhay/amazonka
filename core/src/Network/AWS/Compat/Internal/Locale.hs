{-# LANGUAGE CPP #-}

-- Module      : Network.AWS.Data.Compat.Internal.Locale
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Compat.Internal.Locale
    ( defaultTimeLocale
    , iso8601DateFormat
    ) where

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format (defaultTimeLocale, iso8601DateFormat)
#else
import           System.Locale    (defaultTimeLocale, iso8601DateFormat)
#endif

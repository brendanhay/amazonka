{-# LANGUAGE CPP #-}

-- Module      : Network.AWS.Compat.Internal.Time
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Compat.Internal.Time
    ( parseTime
    ) where

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format (ParseTime, TimeLocale, parseTimeM)

parseTime :: ParseTime a => TimeLocale -> String -> String -> Maybe a
parseTime = parseTimeM True
#else
import           Data.Time.Format (parseTime)
#endif

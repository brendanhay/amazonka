{-# LANGUAGE CPP #-}

-- |
-- Module      : Network.AWS.Compat.Time
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Compat.Time
    ( parseTime
    ) where

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format (ParseTime, TimeLocale, parseTimeM)

parseTime :: ParseTime a => TimeLocale -> String -> String -> Maybe a
parseTime = parseTimeM True
#else
import           Data.Time.Format (parseTime)
#endif

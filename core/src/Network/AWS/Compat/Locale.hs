{-# LANGUAGE CPP #-}

-- |
-- Module      : Network.AWS.Compat.Locale
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Compat.Locale
    ( defaultTimeLocale
    , iso8601DateFormat
    ) where

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format (defaultTimeLocale, iso8601DateFormat)
#else
import           System.Locale    (defaultTimeLocale, iso8601DateFormat)
#endif

-- Module      : Network.AWS.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Monadic where


    -- start r = maybe (return ()) (timer r <=< delay)

    -- delay n = truncate . diffUTCTime n <$> getCurrentTime

    -- -- FIXME:
    -- --  guard against a lower expiration than the -60
    -- --  remove the error . show shenanigans
    -- timer r n = void . forkIO $ do
    --     threadDelay $ (n - 60) * 1000000
    --     !a@Auth{..} <- eitherT throwIO return auth
    --     atomicWriteIORef (_authRef r) a
    --     start r _authExpiry
    -- !a@Auth{..} <- auth
    -- runIO $ do
    --     r <- newAuth a
    --     start r _authExpiry
    --     return r

-- The IONewRef wrapper + timer is designed so that multiple concurrenct
-- accesses of 'Auth' from the 'AWS' environment are not required to calculate
-- expiry and sequentially queue to update it.
--
-- The forked timer ensures a singular owner and pre-emptive refresh of the
-- temporary session credentials.

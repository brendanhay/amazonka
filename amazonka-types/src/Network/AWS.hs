-- Module      : Network.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS where

paginate :: (MonadResource m, AWSRequest a, AWSPager a)
         => Auth
         -> a
         -> Source m (Either (Er a) (Rs a))
paginate = undefined

send :: (MonadResource m, AWSRequest a)
     => Auth
     -> a
     -> m (Either (Er a) (Rs a))
send = undefined

-- async :: MonadBaseControl IO m => 
-- async = undefined

-- wait

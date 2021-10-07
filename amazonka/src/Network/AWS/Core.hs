-- |
-- Module      : Network.AWS.Core
-- Copyright   : (c) 2013-2021 Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Types and values referenced by generated code.
module Network.AWS.Core
  ( module Network.AWS.Types,
    module Network.AWS.Endpoint,
    module Network.AWS.Data,
    module Network.AWS.Pager,
    module Network.AWS.Waiter,
    module Network.AWS.Error,
    (.!@),
    may,
  )
where

import Network.AWS.Data
import Network.AWS.Endpoint
import Network.AWS.Error
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Types
import Network.AWS.Waiter

-- Legacy code generation operators

infixl 7 .!@

(.!@) :: Functor f => f (Maybe a) -> a -> f a
f .!@ x = fromMaybe x <$> f

may :: Applicative f => ([a] -> f b) -> [a] -> f (Maybe b)
may _ [] = pure Nothing
may f xs = Just <$> f xs

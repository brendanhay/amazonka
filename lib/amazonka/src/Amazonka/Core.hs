-- |
-- Module      : Amazonka.Core
-- Copyright   : (c) 2013-2021 Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Types and values referenced by generated code.
module Amazonka.Core
  ( module Amazonka.Types,
    module Amazonka.Endpoint,
    module Amazonka.Data,
    module Amazonka.Pager,
    module Amazonka.Waiter,
    module Amazonka.Error,
    (.!@),
    may,
  )
where

import Amazonka.Data
import Amazonka.Endpoint
import Amazonka.Error
import Amazonka.Pager
import Amazonka.Prelude
import Amazonka.Types
import Amazonka.Waiter

-- Legacy code generation operators

infixl 7 .!@

(.!@) :: Functor f => f (Maybe a) -> a -> f a
f .!@ x = fromMaybe x <$> f

may :: Applicative f => ([a] -> f b) -> [a] -> f (Maybe b)
may _ [] = pure Nothing
may f xs = Just <$> f xs
